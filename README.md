# Dagger Editor — Mechanics Notes

A bottom-up walkthrough of how the editor works, in the order the pieces are built on top of each other.

---

## 1. Raw Mode

Out of the box, the terminal runs in **cooked (canonical) mode**: input is line-buffered, Ctrl-C sends SIGINT, characters are echoed automatically, and the OS translates `\n` to `\r\n` on output. That's fine for a shell, but useless for an editor.

`enableRawMode()` calls `tcgetattr` to snapshot the current settings, then `tcsetattr` to change them. The key flags:

| Flag | Location | Effect when cleared |
|---|---|---|
| `ICANON` | `c_lflag` | Read byte-by-byte instead of line-by-line |
| `ECHO` | `c_lflag` | Don't echo typed characters |
| `ISIG` | `c_lflag` | Ctrl-C / Ctrl-Z don't send signals |
| `IXON` | `c_iflag` | Ctrl-S / Ctrl-Q don't do flow control |
| `ICRNL` | `c_iflag` | CR is not translated to NL |
| `OPOST` | `c_oflag` | No output translation (e.g. LF → CRLF) |

`VMIN=0, VTIME=1` makes `read()` return after 100 ms with whatever bytes arrived, giving us a lightweight non-blocking read without polling.

`disableRawMode()` is registered with `atexit()`, so the terminal is always restored on exit — even when `die()` calls `exit(1)`.

---

## 2. Reading Keys

`editorReadKey()` wraps `read()` in a spin loop (returns on `EAGAIN`, exits on real errors). Single-byte keys are returned directly.

**Escape sequences** are the tricky part. Arrow keys and special keys don't send a single byte — the terminal sends multi-byte sequences starting with `\x1b` (ESC, `0x1B`). For example:

```
Arrow Up  →  ESC [ A   (3 bytes)
Page Up   →  ESC [ 5 ~ (4 bytes)
Home      →  ESC [ H   or  ESC O H  (terminal-dependent)
```

The function reads up to 3 bytes after the ESC and pattern-matches them. If the sequence is unrecognised (or the user simply pressed Escape), it returns the raw ESC byte. This works because `VTIME=1` means the second/third `read()` calls timeout quickly if there's nothing more to read.

The `enum editorKey` assigns values ≥ 1000 to these special keys so they don't collide with any ASCII byte value (0–255).

---

## 3. Data Model: `erow` and Dual Buffers

Each line is an `erow` with **two** representations:

```
chars  / size   — raw file bytes (what you'd write back to disk)
render / rsize  — display bytes  (tabs expanded to spaces)
```

Why two? Because the editor shouldn't mutate the source text just to display it. Tab expansion is a display concern. Keeping them separate also means:

- Saving the file is trivial: just write `chars`.
- Re-rendering after an edit is cheap: call `editorUpdateRow()`, which rebuilds `render` from `chars`.

**Tab expansion** in `editorUpdateRow()`: each `\t` is replaced by spaces up to the next multiple of `DAGGER_TAB_STOP`. For a tab stop of 4:

```
position 0 → 4 spaces
position 1 → 3 spaces
position 3 → 1 space  (already near the next stop)
```

---

## 4. The cx / rx Split

The cursor has two column coordinates:

- **`cx`** — column index into `chars` (file space). This is what arrow keys move.
- **`rx`** — column index into `render` (screen space). This is what the terminal sees.

`editorRowCxToRx()` converts between them by walking `chars[0..cx)` and counting tab expansions. This is called every frame in `editorScroll()` to keep `rx` up to date.

Without this, a cursor sitting after a tab would be positioned in the wrong column on screen.

---

## 5. The Append Buffer

All screen output is accumulated into a single `abuf` (append buffer) and flushed with **one** `write()` call at the end of `editorRefreshScreen()`.

Why? Because a sequence of small `write()` calls causes the terminal to render partial states, producing visible flicker. One atomic write means the terminal sees the full new frame all at once.

`abuf` is a minimal dynamic string:

```c
struct abuf { char *b; int len; };
```

`abAppend()` uses `realloc()` to grow it. `abFree()` releases it. That's the whole pattern.

---

## 6. Scrolling

`editorScroll()` ensures the cursor stays within the visible viewport by adjusting `rowoff` (vertical) and `coloff` (horizontal):

```
if cursor above top    → rowoff = cy
if cursor below bottom → rowoff = cy - screenrows + 1
if cursor left of view → coloff = rx
if cursor right of view → coloff = rx - screencols + 1
```

`editorDrawRows()` then uses `rowoff` and `coloff` as slice offsets when reading from `E.row[filerow].render`.

---

## 7. Screen Rendering

`editorRefreshScreen()` issues this sequence every frame:

1. `ESC[?25l` — **hide cursor** (prevents cursor flicker during redraw)
2. `ESC[H`    — **home cursor** (top-left, 1-based)
3. Draw each visible row (file content or `~` gutter)
4. Draw status bar (inverted video via `ESC[7m` / `ESC[m`)
5. Draw message bar (auto-expires after 5 seconds)
6. `ESC[row;colH` — **position cursor** at the computed screen coordinate
7. `ESC[?25h` — **show cursor**
8. Single `write()` of the entire buffer

All escape sequences used are **VT100/ANSI** — the de facto standard supported by virtually every modern terminal emulator.

---

## 8. Input Dispatch

`editorProcessKeypress()` is the event loop's action layer. It reads one key via `editorReadKey()` and dispatches:

- `Ctrl-Q` → clear screen + exit
- `HOME` / `END` → jump cx to 0 or row end
- `PAGE_UP` / `PAGE_DOWN` → snap cy to viewport edge, then simulate a screenful of arrow presses
- Arrow keys → `editorMoveCursor()`

`editorMoveCursor()` handles line-wrap: pressing Left at column 0 moves up to the end of the previous line, and Right at end-of-line moves down to the start of the next.

---

## 9. The Main Loop

```c
enableRawMode();
initEditor();       // zero state, measure terminal, reserve 2 rows for bars
editorOpen(argv[1]); // load file if given
while (1) {
    editorRefreshScreen();   // draw current state
    editorProcessKeypress(); // block until key, update state
}
```

The loop is intentionally simple: **draw → wait for input → update state → repeat**. All complexity is pushed into the functions; the loop itself is a clean read.

---

## Known Bug

In `getWindowSize()`:

```c
if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 1 || ws.ws_col == 0)
```

`ioctl` returns `0` on success and `-1` on error. The condition should be `== -1`. As written, the fallback (cursor-movement trick) is *always* triggered because `ioctl` succeeds and returns `0`, which is not `1`. The editor still works because `getCursorPosition()` correctly determines the window size, but it does so via a slower fallback path every time.

---

## What's Next

The editor currently supports:
- [x] Raw mode terminal control
- [x] File loading and display
- [x] Scrolling (vertical + horizontal)
- [x] Cursor movement with line-wrap

Obvious next steps beyond:
- [ ] Inserting and deleting characters (`editorInsertChar`, `editorDelChar`)
- [ ] Saving (`Ctrl-S`, `editorSave`)
- [ ] Search (`Ctrl-F`)
- [ ] Syntax highlighting
- [ ] Multiple buffers / split views (longer-term)
