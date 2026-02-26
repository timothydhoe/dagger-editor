/* https://viewsourcecode.org/snaptoken/kilo/06.search.html */

/*** includes ***/

/*
* Feature-test macros: tell the C library to expose POSIX/GNU/BSD extensions
* that aren't part of strict ISO C. Required for getline(), strdup(), etc.
*/
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>		// iscntrl()
#include <errno.h>		// errno, EAGAIN
#include <fcntl.h>		// open(), O_RDWR, O_CREAT
#include <stdio.h>		// FILE, open, getline, snprintf, perror, ...
#include <stdarg.h>		// va_list, va_start, va_end -- variadic functions
#include <stdlib.h>		// malloc, realloc, free, exit, atexit
#include <string.h>		// memcpy, strlen, strdup, ...
#include <sys/ioctl.h>	// ioctl, TIOCGWINSZ -- query terminal window size
#include <sys/types.h>	// ssize_t
#include <termios.h> 	// tcgetattr, tcsetattr, struct termios
#include <time.h>		// time_t, time() -- used for status message timeout
#include <unistd.h>		// read, write, STDIN_FILENO, STDOUT_FILENO


/*** defines ***/
/**************/

#define DAGGER_VERSION "0.1.0"

/*
* Number of spaces a tab character visually expands to.
* The render buffer uses this to convert '\t' → spaces.
*/
#define DAGGER_TAB_STOP 4

#define DAGGER_QUIT_TIMES 3

/*
* Bit-mask trick: ASCII control characters are the first 32 values (0x00–0x1F).
* Ctrl+<key> sets bits 5 & 6 to zero, which is equivalent to AND-ing with 0x1F.
* e.g., CTRL_KEY('q') == 0x11 == 17, which is what the terminal sends.
*/
#define CTRL_KEY(k) ((k) & 0x1f)

/*
* Keys that don't fit in a single byte (arrow keys, Page Up/Down, etc.)
* are represented as integers >= 1000, safely outside the 0–255 ASCII range.
* editorReadKey() parses multi-byte escape sequences and returns these values.
*/
enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum editorHighlight {
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/
/************/

struct editorSyntax {
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;
};

/*
* erow — Editor Row.
* Stores one line of the file in two forms:
*   chars  / size  — the raw file content (may contain '\t', etc.)
*   render / rsize — the display-ready content (tabs expanded to spaces)
*
* Keeping them separate means we never mutate the source data when rendering,
* and we can cheaply recalculate the rendered form after edits.
*/
typedef struct erow {
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    int hl_open_comment;
} erow;

/*
* editorConfig — the single global editor state.
* Using a single struct keeps all state explicit and easy to pass around.
*/
struct editorConfig {
    int cx, cy;				// cursor col/row in the file coordinate space (chars)
    int rx;					// cursor col in the render coordinate space (for tab widths)
    int rowoff;				// first file row currently visible (vertical scroll offset)
    int coloff;				// first render col currently visible (horizontal scroll offset)
    int screenrows;			// terminal height in rows (adjusted for status + msg bars)
    int screencols;			// terminal width in columns
    int numrows;			// number of rows loaded from file
    erow *row;				// heap-allocated array of erow structs
    int dirty;
    char *filename;			// heap-allocated copy of the opened filename (NULL if new file)
    char statusmsg[80];		// short message displayed in the message bar
    time_t statusmsg_time;	// counts when the status message was set (expires msg after 5s)
    struct editorSyntax *syntax; // pointer to the current editorSyntax struct in global editor state
    struct termios orig_termios; // snapshot of terminal settings before entering raw mode
};

// Single global instance. In larger project, pass this by pointer.
struct editorConfig E;

/*** filetypes ***/
/*****************/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "class", "case",
    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", NULL
};

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/
/*****************/
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen(void);
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/
/***************/

/*
* die() — fatal error handler.
* Clears the screen before printing the error so the terminal isn't left in a
* broken state. perror() prints the system error string for errno.
*
* "\x1b[2J" — VT100 erase entire display
* "\x1b[H"  — move cursor to top-left (row 1, col 1)
*/
void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

/*
* disableRawMode() — restores the terminal's original settings.
* Registered with atexit() so it runs automatically on normal exit or die().
* TCSAFLUSH: apply after draining output and discarding pending input.
*/
void disableRawMode(void) {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) die ("tcsetattr");
}

/*
* enableRawMode() — switches the terminal from cooked (line-buffered) mode to
* raw mode so we can read individual keystrokes immediately.
*
* What we're turning OFF (clearing bits):
*   c_iflag:
*     BRKINT  — break condition no longer sends SIGINT
*     ICRNL   — no automatic CR→NL translation (we handle \r ourselves)
*     INPCK   — parity checking off
*     ISTRIP  — don't strip the 8th bit of each byte
*     IXON    — disable XON/XOFF flow control (Ctrl-S / Ctrl-Q pass through)
*   c_oflag:
*     OPOST   — disable all output post-processing (e.g. LF→CRLF translation)
*   c_lflag:
*     ECHO    — don't echo typed characters back to screen (we draw them ourselves)
*     ICANON  — disable canonical (line-buffered) mode; read byte-by-byte
*     IEXTEN  — disable Ctrl-V literal-next and other extended input processing
*     ISIG    — don't convert Ctrl-C / Ctrl-Z into signals
*
* What we're turning ON (setting bits):
*   c_cflag:
*     CS8     — 8-bit character size (usually already set; belt-and-suspenders)
*
* VMIN=0, VTIME=1:
*   read() returns immediately if there's input, or after 100ms if there isn't.
*   This gives us a non-blocking feel without busy-waiting.
*/
void enableRawMode(void) {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die ("tcgetattr");
    atexit(disableRawMode);

    struct termios raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

    raw.c_cc[VMIN] = 0;		// min bytes before read() returns
    raw.c_cc[VTIME] = 1;	// timeout int tenths fo seconds (1 = 100ms)
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

/*
* editorReadKey() — blocks until a keypress is available, then returns it.
*
* Single-byte keys are returned as their ASCII value.
* Arrow keys and special keys arrive as multi-byte escape sequences:
*
*   ESC [ A  →  ARROW_UP       ESC [ 5 ~  →  PAGE_UP
*   ESC [ B  →  ARROW_DOWN     ESC [ 6 ~  →  PAGE_DOWN
*   ESC [ C  →  ARROW_RIGHT    ESC [ 1 ~  →  HOME_KEY
*   ESC [ D  →  ARROW_LEFT     ESC [ 4 ~  →  END_KEY
*   ESC [ H  →  HOME_KEY       ESC O H    →  HOME_KEY (alternate)
*   ESC [ F  →  END_KEY        ESC O F    →  END_KEY  (alternate)
*
* If we get an ESC but can't read a complete sequence (e.g. the user just
* pressed Escape), we return the raw ESC byte ('\x1b').
*
* EAGAIN is ignored: on some systems (Cygwin) VTIME timeout makes read() return
* -1/EAGAIN rather than 0.
*/
int editorReadKey(void) {
    int nread;
    char c;

    // Spin until we get exactly one byte.
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }
    
    if (c == '\x1b') {
        char seq[3];

        // Try to read the next two bytes of the sequences within ~100ms.
        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                // Extended sequence: ESC [ <digit> ~
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '2': return END_KEY;
                        case '3': return DEL_KEY; // not implemented yet.
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            } else {
                // Short sequences: ESC [ <letter>
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            // VT220 alternate sequences: ESC O H / ESC O F
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }

        return '\x1b'; // Unrecognised escape sequence; return raw ESC.
    } else {
    return c;
    }
}

/*
* getCursorPosition() — fallback for terminals that don't support TIOCGWINSZ.
*
* Sends ESC [ 6 n  (Device Status Report — cursor position).
* The terminal responds with  ESC [ <row> ; <col> R
* We read that response and parse it into *rows and *cols.
*/
int getCursorPosition(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    // Request cursor position.
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return 1;

    // Read response characters until we see 'R' (end of sequence)
    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    // Validate the response starts with ESC [
    if (buf[0] != '\x1b' || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

    return 0;
}

/*
* getWindowSize() — queries the terminal dimensions.
*
* Primary path: ioctl(TIOCGWINSZ) fills a winsize struct directly.
* Fallback: move cursor to a huge position (999,999), then ask where it ended
*           up — the terminal clamps it to the last row/column, giving us the
*           window size indirectly.
*
* Note: the ioctl success check is `== 1` which looks like a bug — it should
* probably be `== -1`. (ioctl returns 0 on success, -1 on error.)
* TODO: fix the ioctl error check condition (== 1 should be == -1).
*/
int getWindowSize(int *rows, int *cols) {
    struct winsize ws;

    // cursors up and down documentation:
    // https://vt100.net/docs/vt100-ug/chapter3.html#CUD
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 1 || ws.ws_col == 0) {
        /* Fallback: cursor-movement trick.
         * ESC [ 999 C  — move cursor 999 columns right (clamps to last col)
         * ESC [ 999 B  — move cursor 999 rows down    (clamps to last row)
         */
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

/*** syntax highlighting ***/
/**************************/

int is_separator(int c) {
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if (E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;

    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->render[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                i += mce_len;
                in_comment = 0;
                prev_sep = 1;
                continue;
                } else {
                i++;
                continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) in_string = 0;
                i++;
                prev_sep = 1;
                continue;
            } else {
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;
                
                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {
                memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                i += klen;
                break;
                }
            }
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

        prev_sep = is_separator(c);
        i++;
    }

    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows)
        editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
    // https://en.wikipedia.org/wiki/ANSI_escape_code
    switch (hl) {
    case HL_COMMENT:
    case HL_MLCOMMENT: return 36;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 32;
    case HL_STRING: return 35;
    case HL_NUMBER: return 31;
    case HL_MATCH: return 34;
    default: return 37;
    }
}

void editorSelectSyntaxHighlight(void) {
    E.syntax = NULL;
    if (E.filename == NULL) return;

    char *ext = strrchr(E.filename, '.');

    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;

                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }

                return;
            }
            i++;
        }
    }
}


/*** row operations ***/
/*********************/

/*
* editorRowCxToRx() — converts a file-column index (cx) to a render-column
* index (rx), accounting for tab expansion.
*
* For each character before cx:
*   - If it's a tab, advance rx to the next tab stop.
*   - Otherwise, increment rx by 1.
*
* This is necessary because tabs are stored as single '\t' chars in `chars`
* but displayed as multiple spaces in `render`.
*/
int editorRowCxToRx(erow *row, int cx) {
    int rx = 0;
    int j;
    for (j = 0; j < cx; j++) {
        if (row->chars[j] == '\t')
            rx += (DAGGER_TAB_STOP - 1) - (rx % DAGGER_TAB_STOP); // spaced to next tab stop
        rx++;
    }
    return rx;
}

int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (DAGGER_TAB_STOP - 1) - (cur_rx % DAGGER_TAB_STOP);
        cur_rx++;

        if (cur_rx > rx) return cx;
    }
    return cx;
}

/*
* editorUpdateRow() — rebuilds the render buffer from chars.
*
* Allocates enough space for worst-case tab expansion:
*   each '\t' can expand to at most DAGGER_TAB_STOP spaces.
* Then copies chars, replacing each '\t' with the right number of spaces to
* align to the next tab stop.
*
* Call this whenever a row's chars content changes.
*/
void editorUpdateRow(erow *row) {
    // Count tabs to know how much extra space to allocate.
    int tabs = 0;
    int j;
    for (j = 0; j < row->size; j++)
        if (row->chars[j] == '\t') tabs ++;

    free(row->render);
    /*
     * Worst case: each tab becomes DAGGER_TAB_STOP spaces.
     * We already have 1 byte for each tab in row->size, so we need
     * (DAGGER_TAB_STOP - 1) extra bytes per tab. +1 for NUL terminator.
     */
    row->render = malloc(row->size + tabs * (DAGGER_TAB_STOP - 1) + 1);
    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            // Pad with spaces until idx hits a tab stop.
            while (idx % DAGGER_TAB_STOP != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

/*
* editorInsertRow() — adds a new row at the end of E.row[].
*
* Grows the row array with realloc, then copies the raw string s into the
* new erow's chars buffer and calls editorUpdateRow to populate render.
*/
void editorInsertRow(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    // Grow the row array by one slot.
    E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
    for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hl_open_comment = 0;
    editorUpdateRow(&E.row[at]); // Build the render buffer immediately.

    E.numrows++;
    E.dirty++;
}

void editorFreeRow(erow *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    if (at < 0 || at >= E.numrows) return;
    editorFreeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
    for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
    E.numrows--;
    E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
    E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
    if (at < 0 || at >= row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editorUpdateRow(row);
    E.dirty++;
}


/*** editor operations ***/
/************************/

void editorInsertChar(int c) {
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0);
    }
    editorRowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void editorInsertNewLine(void) {
    if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }
    E.cy++;
    E.cx = 0;
}

void editorDelChar(void) {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;

    erow *row = &E.row[E.cy];
    if (E.cx > 0) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].size;
        editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        editorDelRow(E.cy);
        E.cy--;
    }
}


/*** file i/o ***/
/***************/

char *editorRowsToString(int *buflen) {
    int totlen = 0;
    int j;
    for (j = 0; j < E.numrows; j++)
        totlen += E.row[j].size + 1;
    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for (j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

/*
* editorOpen() — loads a file from disk into the editor's row array.
*
* Uses getline() which allocates the line buffer for us (we free it at the
* end). Strips trailing '\n' and '\r' so rows don't carry newline characters.
*/
void editorOpen(char *filename) {
    free(E.filename);
    E.filename = strdup(filename); // Create a heap copy of the filename.

    editorSelectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;	// getline() will allocate and grow this buffer
    size_t linecap = 0;	// current allocated capacity (managed by getline())
    ssize_t linelen;
    while ((linelen = getline(&line, &linecap, fp)) != -1) {
        // Strip trailing newline characters.
        while (linelen > 0 && (line[linelen - 1] == '\n' ||
                               line[linelen - 1] == '\r'))
            linelen--;
        editorInsertRow(E.numrows, line, linelen);
    }
    free(line); // Free the buffer allocated by getline.
    fclose(fp);
    E.dirty = 0;
}

void editorSave(void) {
    if (E.filename == NULL) {
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if (E.filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }
        editorSelectSyntaxHighlight();
    }

    int len;
    char *buf = editorRowsToString(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                editorSetStatusMessage("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    editorSetStatusMessage("Can't Save! I/O error: %s", strerror(errno));
}

/*** find ***/
/***********/

void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    static int saved_hl_line;
    static char *saved_hl = NULL;

    if (saved_hl) {
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;
    int i;
    for (i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;

        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editorRowRxToCx(row, match - row->render);
            E.rowoff = E.numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void editorFind(void) {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}



/*** append buffer ***/
/********************/

/*
* abuf — a simple dynamic string (append-only).
*
* Problem: calling write() once per character or element would cause many
* small syscalls, flickering the terminal.
* Solution: accumulate all output in a single buffer (abuf), then write()
* the whole thing in one shot at the end of editorRefreshScreen().
*
* This is a minimal arena/string-builder pattern.
*/
struct abuf {
    char *b;	// heap-allocated buffer
    int len;	// currently used length
};

// Empty initialiser (using compound literal / designated initialiser)
#define ABUF_INIT {NULL, 0}

/*
* abAppend() — appends s (len bytes) to the buffer, growing it with realloc.
* On allocation failure we silently drop the data (terminal output is
* best-effort; a missed frame is better than crashing).
*/
void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

/* abFree() — releases the buffer. Always pair with a matching abAppend loop. */
void abFree(struct abuf *ab) {
    free(ab->b);
}

/*** output ***/
/*************/

/*
* editorScroll() — adjusts rowoff and coloff so the cursor stays on screen.
*
* Also computes E.rx (render-column for the cursor) from E.cx,
* because the status bar and cursor-positioning escape need render coords.
*
* The four if-blocks handle the four scroll directions:
*   cursor above visible area  → scroll up   (decrease rowoff)
*   cursor below visible area  → scroll down (increase rowoff)
*   cursor left of visible area  → scroll left  (decrease coloff)
*   cursor right of visible area → scroll right (increase coloff)
*/
void editorScroll(void) {
    // Compute render-x for the current cursor position.
    E.rx = E.cx;
    if (E.cy < E.numrows) {
        E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
    }

    // Vertical scroll
    if (E.cy < E.rowoff) {
        E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
        E.rowoff = E.cy - E.screenrows + 1;
    }
    // Horizontal scroll
    if (E.rx < E.coloff) {
        E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
        E.coloff = E.rx - E.screencols + 1;
    }
}

/*
* editorDrawRows() — writes each visible screen row to the append buffer.
*
* Three cases per row:
*   1. Row is past end of file: draw a '~' gutter marker.
*      Special case: if the buffer is empty and this is the upper-third row,
*      draw a centred welcome message instead.
*   2. Row has file content: write the visible slice of the render buffer
*      (accounting for horizontal scroll via coloff).
*
* Each row ends with:
*   ESC [ K  — erase from cursor to end of line (clears stale characters)
*   \r\n     — move to the start of the next line
*             (OPOST is disabled so we must send \r explicitly)
*/
void editorDrawRows(struct abuf *ab) {
    int y;
    for (y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff; // Map screen row -> file row.
        if (filerow >= E.numrows) {
            // Past end of file.
            if (E.numrows == 0 && y == E.screenrows / 3) {
                // Welcome message, centered horizontally
                char welcome[80];
                int welcomelen = snprintf(welcome, sizeof(welcome),
                        "Dagger editor  †  version %s", DAGGER_VERSION);
                if (welcomelen > E.screencols) welcomelen = E.screencols;

                int padding = (E.screencols - welcomelen) / 2;

                if (padding) {
                    abAppend(ab, "~", 1);
                    padding--;
                }

                while (padding--) abAppend(ab, " ", 1);
                abAppend(ab, welcome, welcomelen);
            } else {
                abAppend(ab, "~", 1);
            }
        } else {
            // File content: clip to visible horizontal window.
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) len = 0;
            if (len > E.screencols) len = E.screencols;
            char *c = &E.row[filerow].render[E.coloff];
            unsigned char *hl = &E.row[filerow].hl[E.coloff];
            int current_color = -1;
            int j;
            for (j = 0; j < len; j++) {
                if (iscntrl(c[j])) {
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if (current_color != -1) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
                        abAppend(ab, buf, clen);
                    }
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    if (color != current_color) {
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                        abAppend(ab, buf, clen);
                        }
                        abAppend(ab, &c[j], 1);
                    }
                }
                abAppend(ab, "\x1b[39m", 5);
            }

        abAppend(ab, "\x1b[K", 3);	// Erase next of line.
        abAppend(ab, "\r\n", 2);	// Next row.
    }
}


/*
*editorDrawStatusBar() — draws a one-line inverted-video status bar.
*
* ESC [ 7 m  — Select Graphic Rendition: invert foreground/background colours
* ESC [ m    — Reset all attributes (back to normal)
*
* Left side: filename (up to 20 chars) + line count.
* Right side: current line / total lines, right-aligned.
*/
void editorDrawStatusBar(struct abuf *ab) {
    // m command for Select Graphic Rendition:
    // https://vt100.net/docs/vt100-ug/chapter3.html#SGR
    abAppend(ab, "\x1b[7m", 4);
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s † %d lines %s",
            E.filename ? E.filename : "[No Name]", E.numrows, E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
               E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);

    /*
     * Pad the gap between left and right status with spaces.
     * When exactly rlen columns remain, append the right status and stop.
     */
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
        abAppend(ab, " ", 1);
        len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);	// Reset video attributes
    abAppend(ab, "\r\n", 2);
}

/*
* editorDrawMessageBar() — draws the one-line message bar below the status bar.
*
* The message is displayed for 5 seconds (compared against statusmsg_time),
* then silently cleared by not appending anything.
*/
void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3); // Clear the line first
    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5)
        abAppend(ab, E.statusmsg, msglen);
}

/*
* editorRefreshScreen() — redraws the entire screen in one write() call.
*
* Sequence:
*   1. Recompute scroll offsets (editorScroll).
*   2. Hide the cursor (prevents flicker during redraw).
*   3. Move cursor to top-left (ESC [ H).
*   4. Draw all content rows, status bar, message bar into the abuf.
*   5. Position the cursor at (cy - rowoff, rx - coloff) — screen coords.
*   6. Show the cursor again.
*   7. Flush the entire buffer with a single write().
*
* Using a single write() avoids the visible partial-update flicker you'd get
* from many small writes interleaved with rendering computation.
*/
void editorRefreshScreen(void) {
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);	// hide cursor (DEC private mode 25)
    abAppend(&ab, "\x1b[H", 3);		// cursor to home position

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    /*
     * Position cursor: ESC [ <row> ; <col> H
     * +1 because VT100 cursor coordinates are 1-based.
     * Subtract offsets to convert file coords → screen coords.
     */
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH",(E.cy - E.rowoff) + 1,
                                             (E.rx - E.coloff) + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6); // show cursor

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

/*
* editorSetStatusMessage() — sets E.statusmsg with a printf-style format.
*
* Uses vsnprintf so the caller can pass format strings + variadic args,
* just like printf. Records the current time so the message auto-expires.
*/
void editorSetStatusMessage(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}


/*** input ***/
/************/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        editorSetStatusMessage(prompt, buf);
        editorRefreshScreen();

        int c = editorReadKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') {
            editorSetStatusMessage("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
            buf[buflen++] = c;
            buf[buflen] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

/*
* editorMoveCursor() — updates E.cx / E.cy based on a directional key.
*
* Arrow left/right: move within the current line, wrapping to
*   previous/next line at the beginning/end of a line.
* Arrow up/down: move between lines, clamped to [0, numrows].
*
* After changing cy, the cursor column is clamped to the new row's length
* (so moving from a long line to a short one doesn't leave cx out-of-bounds).
*/
void editorMoveCursor(int key) {
    // Get the current row, or NULL if the cursor is past the last row.
    erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    switch(key) {
        case ARROW_LEFT:
            if (E.cx != 0){
                E.cx--;
            } else if (E.cy > 0) {
                // Wrap to end of the previous line.
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                // wrap to beginning of the next line.
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if (E.cy != 0) {
                E.cy--;
            }
            break;
        case ARROW_DOWN:
            if (E.cy != E.numrows) {
                E.cy++; // One past last row = "after EOF"
            }
            break;
    }

    // Clamp cx to the new row's length (handles moveing to a shorter line)
    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen) {
        E.cx = rowlen;
    }

}

/*
* editorProcessKeypress() — dispatches a key to the appropriate action.
*
* This is the editor's central input event handler. Every key press flows
* through here. As the editor grows, new key bindings live here.
*/
void editorProcessKeypress(void) {
    static int quit_times = DAGGER_QUIT_TIMES;

    int c = editorReadKey();

    switch(c) {
        case '\r':
            editorInsertNewLine();
            break;

        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editorSetStatusMessage("Warning!!! File has unsaved changes. "
                        "Press Ctrl-Q %d more times to quit.", quit_times);
                quit_times--;
                return;
            }
            // Quit: clear screen en exit cleanlydd
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSave();
            break;

        case HOME_KEY:
            E.cx = 0; // jump to start of line
            break;

        case END_KEY:
            if (E.cy < E.numrows)
                E.cx = E.row[E.cy].size;  // jump to end of line
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
            editorDelChar();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
            {
                /*
                 * Snap E.cy to the top/bottom of the current viewport first,
                 * then simulate a full screen's worth of arrow key presses.
                 * This keeps scrolling consistent with the scroll logic.
                 */
                if (c == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }
                int times = E.screenrows;
                while (times--)
                    editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            break;

        default:
            editorInsertChar(c);
            break;
    }

    quit_times = DAGGER_QUIT_TIMES;
}


/*** init ***/
/***********/

/*
* initEditor() — zeroes out the global state and queries the terminal size.
*
* E.screenrows is reduced by 2 to reserve space for:
*   - the status bar (1 line)
*   - the message bar (1 line)
*/
void initEditor(void) {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.screenrows -= 2;	// Reserve 2 rows for status bar + message bar
}


/*
* main() — entry point.
*
* 1. enableRawMode(): take over the terminal.
* 2. initEditor(): set up state, measure terminal.
* 3. editorOpen(): load file if one was passed on the command line.
* 4. editorSetStatusMessage(): show the help hint.
* 5. Event loop: refresh screen → handle one keypress → repeat.
*/
int main(int argc, char *argv[]) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorOpen(argv[1]);
    }

    editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editorRefreshScreen();
        editorProcessKeypress();
    }
    return 0;
}
