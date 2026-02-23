"""
x86-32 Assembly Code Editor
tkinter-based GUI with syntax highlighting, line numbers, and assembler integration.
"""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox, simpledialog
import os
import re

from assembler import Assembler


# ---------------------------------------------------------------------------
# Syntax highlighting patterns
# ---------------------------------------------------------------------------

MNEMONICS = {
    'MOV', 'XCHG', 'LEA', 'MOVZX', 'MOVSX', 'PUSH', 'POP',
    'ADD', 'SUB', 'ADC', 'SBB', 'AND', 'OR', 'XOR', 'CMP', 'TEST',
    'INC', 'DEC', 'NEG', 'NOT', 'MUL', 'IMUL', 'DIV', 'IDIV',
    'SHL', 'SHR', 'SAR', 'SAL', 'ROL', 'ROR', 'RCL', 'RCR',
    'JMP', 'JE', 'JNE', 'JZ', 'JNZ', 'JG', 'JGE', 'JL', 'JLE',
    'JA', 'JAE', 'JB', 'JBE', 'JO', 'JNO', 'JS', 'JNS', 'JP', 'JNP',
    'JC', 'JNC', 'JNAE', 'JNB', 'JNBE', 'JNA', 'JNGE', 'JNL', 'JNG', 'JNLE',
    'CALL', 'RET', 'INT',
    'NOP', 'HLT', 'CLI', 'STI', 'CLC', 'STC', 'CLD', 'STD', 'CMC',
    'PUSHAD', 'POPAD', 'PUSHFD', 'POPFD', 'LEAVE',
    'LOOP', 'LOOPE', 'LOOPZ', 'LOOPNE', 'LOOPNZ', 'JECXZ',
    'MOVSB', 'MOVSD', 'STOSB', 'STOSD', 'LODSB', 'LODSD',
    'CMPSB', 'CMPSD', 'SCASB', 'SCASD',
    'REP', 'REPE', 'REPZ', 'REPNE', 'REPNZ', 'LOCK',
    'CBW', 'CWDE', 'CDQ', 'CWD', 'XLAT',
    'SAHF', 'LAHF', 'INT3', 'IRET',
    'AAA', 'AAS', 'DAA', 'DAS',
}

REGISTERS = {
    'EAX', 'EBX', 'ECX', 'EDX', 'ESI', 'EDI', 'ESP', 'EBP',
    'AX', 'BX', 'CX', 'DX', 'SI', 'DI', 'SP', 'BP',
    'AL', 'AH', 'BL', 'BH', 'CL', 'CH', 'DL', 'DH',
    'CS', 'DS', 'ES', 'FS', 'GS', 'SS',
}

DIRECTIVES = {
    'DB', 'DW', 'DD', 'DQ', 'RESB', 'RESW', 'RESD',
    'EQU', 'TIMES', 'SECTION', 'SEGMENT', 'ORG',
    'GLOBAL', 'EXTERN', 'BITS',
    'BYTE', 'WORD', 'DWORD', 'PTR',
}

# Color scheme
COLORS = {
    'mnemonic': '#2060D0',     # blue
    'register': '#D02020',     # red
    'number': '#208020',       # green
    'comment': '#808080',      # gray
    'string': '#D07020',       # orange
    'directive': '#8020A0',    # purple
    'label_def': '#009090',    # cyan/teal
    'background': '#FFFFFF',
    'foreground': '#1E1E1E',
    'line_num_bg': '#F0F0F0',
    'line_num_fg': '#808080',
    'cursor_line': '#FFF8E0',
    'selection': '#BDE0FE',
    'error_line': '#FFE0E0',
}


class LineNumbers(tk.Canvas):
    """Canvas widget showing line numbers, synced with a Text widget."""

    def __init__(self, parent, text_widget, **kwargs):
        super().__init__(parent, **kwargs)
        self.text_widget = text_widget
        self.config(
            width=50,
            bg=COLORS['line_num_bg'],
            highlightthickness=0,
            bd=0,
        )

    def redraw(self):
        self.delete('all')
        i = self.text_widget.index('@0,0')
        while True:
            dline = self.text_widget.dlineinfo(i)
            if dline is None:
                break
            y = dline[1]
            linenum = str(i).split('.')[0]
            self.create_text(
                45, y,
                anchor='ne',
                text=linenum,
                fill=COLORS['line_num_fg'],
                font=('Consolas', 12),
            )
            i = self.text_widget.index(f'{i}+1line')
            if int(i.split('.')[0]) > int(self.text_widget.index('end-1c').split('.')[0]):
                break


class FindReplaceDialog:
    """Modeless find/replace dialog."""

    def __init__(self, parent, editor):
        self.editor = editor
        self.top = tk.Toplevel(parent)
        self.top.title("Find & Replace")
        self.top.geometry("420x160")
        self.top.resizable(False, False)
        self.top.transient(parent)

        frame = ttk.Frame(self.top, padding=10)
        frame.pack(fill='both', expand=True)

        ttk.Label(frame, text="Find:").grid(row=0, column=0, sticky='w', pady=2)
        self.find_var = tk.StringVar()
        self.find_entry = ttk.Entry(frame, textvariable=self.find_var, width=30)
        self.find_entry.grid(row=0, column=1, padx=5, pady=2)
        self.find_entry.focus_set()

        ttk.Label(frame, text="Replace:").grid(row=1, column=0, sticky='w', pady=2)
        self.replace_var = tk.StringVar()
        self.replace_entry = ttk.Entry(frame, textvariable=self.replace_var, width=30)
        self.replace_entry.grid(row=1, column=1, padx=5, pady=2)

        self.case_var = tk.BooleanVar(value=False)
        ttk.Checkbutton(frame, text="Case sensitive", variable=self.case_var).grid(
            row=2, column=0, columnspan=2, sticky='w', pady=2)

        btn_frame = ttk.Frame(frame)
        btn_frame.grid(row=3, column=0, columnspan=2, pady=5)

        ttk.Button(btn_frame, text="Find Next", command=self.find_next).pack(side='left', padx=2)
        ttk.Button(btn_frame, text="Replace", command=self.replace).pack(side='left', padx=2)
        ttk.Button(btn_frame, text="Replace All", command=self.replace_all).pack(side='left', padx=2)
        ttk.Button(btn_frame, text="Close", command=self.top.destroy).pack(side='left', padx=2)

        self.find_entry.bind('<Return>', lambda e: self.find_next())

    def find_next(self):
        pattern = self.find_var.get()
        if not pattern:
            return
        editor = self.editor.code_editor
        editor.tag_remove('found', '1.0', 'end')

        start = editor.index('insert+1c')
        nocase = not self.case_var.get()

        pos = editor.search(pattern, start, stopindex='end', nocase=nocase)
        if not pos:
            pos = editor.search(pattern, '1.0', stopindex=start, nocase=nocase)
        if pos:
            end = f"{pos}+{len(pattern)}c"
            editor.tag_add('found', pos, end)
            editor.tag_config('found', background='#FFFF00')
            editor.mark_set('insert', end)
            editor.see(pos)

    def replace(self):
        pattern = self.find_var.get()
        replacement = self.replace_var.get()
        editor = self.editor.code_editor

        try:
            sel_start = editor.index('sel.first')
            sel_end = editor.index('sel.last')
            sel_text = editor.get(sel_start, sel_end)
            nocase = not self.case_var.get()
            if nocase:
                match = sel_text.lower() == pattern.lower()
            else:
                match = sel_text == pattern
            if match:
                editor.delete(sel_start, sel_end)
                editor.insert(sel_start, replacement)
        except tk.TclError:
            pass
        self.find_next()

    def replace_all(self):
        pattern = self.find_var.get()
        replacement = self.replace_var.get()
        if not pattern:
            return
        editor = self.editor.code_editor
        nocase = not self.case_var.get()

        content = editor.get('1.0', 'end-1c')
        if nocase:
            new_content = re.sub(re.escape(pattern), replacement, content, flags=re.IGNORECASE)
        else:
            new_content = content.replace(pattern, replacement)

        if new_content != content:
            editor.delete('1.0', 'end')
            editor.insert('1.0', new_content)
            self.editor._on_modified()


class AssemblyEditor:
    """Main application window."""

    def __init__(self, root: tk.Tk):
        self.root = root
        self.root.title("x86-32 Assembly Editor")
        self.root.geometry("1100x750")

        self.current_file = None
        self.modified = False
        self.assembler = Assembler()
        self.last_result = None
        self.error_lines = []
        self._highlight_job = None

        self._build_ui()
        self._bind_keys()
        self._update_title()

    # -----------------------------------------------------------------------
    # UI Construction
    # -----------------------------------------------------------------------

    def _build_ui(self):
        # Menu bar
        self._build_menu()

        # Toolbar
        self._build_toolbar()

        # Main paned window (vertical split: editor top, output bottom)
        self.paned = ttk.PanedWindow(self.root, orient='vertical')
        self.paned.pack(fill='both', expand=True)

        # Editor frame (top)
        editor_frame = ttk.Frame(self.paned)
        self.paned.add(editor_frame, weight=7)

        # Line numbers
        self.code_editor = tk.Text(
            editor_frame,
            wrap='none',
            font=('Consolas', 12),
            bg=COLORS['background'],
            fg=COLORS['foreground'],
            insertbackground='#000000',
            selectbackground=COLORS['selection'],
            undo=True,
            maxundo=-1,
            autoseparators=True,
            tabs=('4c',),
            padx=5,
            pady=5,
        )

        self.line_numbers = LineNumbers(editor_frame, self.code_editor)
        self.line_numbers.pack(side='left', fill='y')

        # Scrollbars
        yscroll = ttk.Scrollbar(editor_frame, orient='vertical', command=self._on_yscroll)
        xscroll = ttk.Scrollbar(editor_frame, orient='horizontal', command=self.code_editor.xview)
        self.code_editor.config(yscrollcommand=yscroll.set, xscrollcommand=xscroll.set)

        yscroll.pack(side='right', fill='y')
        xscroll.pack(side='bottom', fill='x')
        self.code_editor.pack(side='left', fill='both', expand=True)

        # Output frame (bottom)
        output_frame = ttk.Frame(self.paned)
        self.paned.add(output_frame, weight=3)

        output_label = ttk.Label(output_frame, text=" Output", font=('Consolas', 10, 'bold'))
        output_label.pack(side='top', fill='x', anchor='w')

        self.output_text = tk.Text(
            output_frame,
            wrap='none',
            font=('Consolas', 11),
            bg='#1E1E2E',
            fg='#E0E0E0',
            insertbackground='#E0E0E0',
            state='disabled',
            height=10,
            padx=5,
            pady=5,
        )
        out_yscroll = ttk.Scrollbar(output_frame, orient='vertical', command=self.output_text.yview)
        out_xscroll = ttk.Scrollbar(output_frame, orient='horizontal', command=self.output_text.xview)
        self.output_text.config(yscrollcommand=out_yscroll.set, xscrollcommand=out_xscroll.set)
        out_yscroll.pack(side='right', fill='y')
        out_xscroll.pack(side='bottom', fill='x')
        self.output_text.pack(fill='both', expand=True)

        # Status bar
        self.status_var = tk.StringVar(value="Ready")
        self.cursor_var = tk.StringVar(value="Ln 1, Col 1")
        status_bar = ttk.Frame(self.root)
        status_bar.pack(side='bottom', fill='x')
        ttk.Label(status_bar, textvariable=self.status_var, relief='sunken', anchor='w').pack(
            side='left', fill='x', expand=True, padx=2)
        ttk.Label(status_bar, textvariable=self.cursor_var, relief='sunken', anchor='e', width=20).pack(
            side='right', padx=2)

        # Configure syntax tags
        self._configure_tags()

        # Bind events
        self.code_editor.bind('<<Modified>>', self._on_text_modified)
        self.code_editor.bind('<KeyRelease>', self._on_key_release)
        self.code_editor.bind('<ButtonRelease-1>', self._on_cursor_move)
        self.code_editor.bind('<Configure>', lambda e: self.line_numbers.redraw())
        self.output_text.bind('<ButtonRelease-1>', self._on_output_click)

    def _build_menu(self):
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)

        # File menu
        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="File", menu=file_menu)
        file_menu.add_command(label="New", command=self.file_new, accelerator="Ctrl+N")
        file_menu.add_command(label="Open...", command=self.file_open, accelerator="Ctrl+O")
        file_menu.add_separator()
        file_menu.add_command(label="Save", command=self.file_save, accelerator="Ctrl+S")
        file_menu.add_command(label="Save As...", command=self.file_save_as, accelerator="Ctrl+Shift+S")
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self.file_exit, accelerator="Alt+F4")

        # Edit menu
        edit_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Edit", menu=edit_menu)
        edit_menu.add_command(label="Undo", command=self._undo, accelerator="Ctrl+Z")
        edit_menu.add_command(label="Redo", command=self._redo, accelerator="Ctrl+Y")
        edit_menu.add_separator()
        edit_menu.add_command(label="Cut", command=self._cut, accelerator="Ctrl+X")
        edit_menu.add_command(label="Copy", command=self._copy, accelerator="Ctrl+C")
        edit_menu.add_command(label="Paste", command=self._paste, accelerator="Ctrl+V")
        edit_menu.add_separator()
        edit_menu.add_command(label="Select All", command=self._select_all, accelerator="Ctrl+A")
        edit_menu.add_command(label="Find...", command=self._show_find, accelerator="Ctrl+F")
        edit_menu.add_command(label="Replace...", command=self._show_replace, accelerator="Ctrl+H")
        edit_menu.add_command(label="Go to Line...", command=self._goto_line, accelerator="Ctrl+G")

        # Build menu
        build_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Build", menu=build_menu)
        build_menu.add_command(label="Assemble", command=self.assemble, accelerator="F5")
        build_menu.add_command(label="Show Listing", command=self.show_listing, accelerator="F6")
        build_menu.add_command(label="Clear Output", command=self.clear_output, accelerator="F7")
        build_menu.add_separator()
        build_menu.add_command(label="Export Listing...", command=self.export_listing)
        build_menu.add_command(label="Export Binary...", command=self.export_binary)

        # Help menu
        help_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Help", menu=help_menu)
        help_menu.add_command(label="Help Topics", command=self._show_help, accelerator="F1")
        help_menu.add_command(label="Instruction Reference", command=self._show_instruction_ref)
        help_menu.add_separator()
        help_menu.add_command(label="About", command=self._show_about)

    def _build_toolbar(self):
        toolbar = ttk.Frame(self.root, padding=(2, 2))
        toolbar.pack(side='top', fill='x')

        btn_style = {'width': 0, 'padding': (8, 4)}

        # File group
        ttk.Button(toolbar, text="New", command=self.file_new, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Open", command=self.file_open, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Save", command=self.file_save, **btn_style).pack(side='left', padx=1)

        ttk.Separator(toolbar, orient='vertical').pack(side='left', fill='y', padx=6, pady=2)

        # Edit group
        ttk.Button(toolbar, text="Undo", command=self._undo, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Redo", command=self._redo, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Find", command=self._show_find, **btn_style).pack(side='left', padx=1)

        ttk.Separator(toolbar, orient='vertical').pack(side='left', fill='y', padx=6, pady=2)

        # Build group
        ttk.Button(toolbar, text="Assemble (F5)", command=self.assemble, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Listing (F6)", command=self.show_listing, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Clear (F7)", command=self.clear_output, **btn_style).pack(side='left', padx=1)

        ttk.Separator(toolbar, orient='vertical').pack(side='left', fill='y', padx=6, pady=2)

        # Export group
        ttk.Button(toolbar, text="Export .lst", command=self.export_listing, **btn_style).pack(side='left', padx=1)
        ttk.Button(toolbar, text="Export .bin", command=self.export_binary, **btn_style).pack(side='left', padx=1)

        ttk.Separator(toolbar, orient='vertical').pack(side='left', fill='y', padx=6, pady=2)

        # Help
        ttk.Button(toolbar, text="Help (F1)", command=self._show_help, **btn_style).pack(side='left', padx=1)

    def _configure_tags(self):
        self.code_editor.tag_configure('mnemonic', foreground=COLORS['mnemonic'], font=('Consolas', 12, 'bold'))
        self.code_editor.tag_configure('register', foreground=COLORS['register'])
        self.code_editor.tag_configure('number', foreground=COLORS['number'])
        self.code_editor.tag_configure('comment', foreground=COLORS['comment'], font=('Consolas', 12, 'italic'))
        self.code_editor.tag_configure('string', foreground=COLORS['string'])
        self.code_editor.tag_configure('directive', foreground=COLORS['directive'], font=('Consolas', 12, 'bold'))
        self.code_editor.tag_configure('label_def', foreground=COLORS['label_def'], font=('Consolas', 12, 'bold'))
        self.code_editor.tag_configure('error_line', background=COLORS['error_line'])

        # Output tags
        self.output_text.tag_configure('error', foreground='#FF6060')
        self.output_text.tag_configure('success', foreground='#60FF60')
        self.output_text.tag_configure('header', foreground='#80C0FF', font=('Consolas', 11, 'bold'))
        self.output_text.tag_configure('clickable', foreground='#FF6060', underline=True)

    def _bind_keys(self):
        self.root.bind('<Control-n>', lambda e: self.file_new())
        self.root.bind('<Control-o>', lambda e: self.file_open())
        self.root.bind('<Control-s>', lambda e: self.file_save())
        self.root.bind('<Control-Shift-S>', lambda e: self.file_save_as())
        self.root.bind('<Control-z>', lambda e: self._undo())
        self.root.bind('<Control-y>', lambda e: self._redo())
        self.root.bind('<Control-f>', lambda e: self._show_find())
        self.root.bind('<Control-h>', lambda e: self._show_replace())
        self.root.bind('<Control-g>', lambda e: self._goto_line())
        self.root.bind('<Control-a>', lambda e: self._select_all())
        self.root.bind('<F5>', lambda e: self.assemble())
        self.root.bind('<F6>', lambda e: self.show_listing())
        self.root.bind('<F7>', lambda e: self.clear_output())
        self.root.bind('<F1>', lambda e: self._show_help())
        self.root.protocol("WM_DELETE_WINDOW", self.file_exit)

    def _on_yscroll(self, *args):
        self.code_editor.yview(*args)
        self.line_numbers.redraw()

    # -----------------------------------------------------------------------
    # Event handlers
    # -----------------------------------------------------------------------

    def _on_text_modified(self, event=None):
        if self.code_editor.edit_modified():
            self._on_modified()
            self.code_editor.edit_modified(False)

    def _on_modified(self):
        self.modified = True
        self._update_title()
        self._schedule_highlight()
        self.line_numbers.redraw()

    def _on_key_release(self, event=None):
        self._on_cursor_move()
        self.line_numbers.redraw()

    def _on_cursor_move(self, event=None):
        pos = self.code_editor.index('insert')
        line, col = pos.split('.')
        self.cursor_var.set(f"Ln {line}, Col {int(col) + 1}")

    def _on_output_click(self, event=None):
        """Navigate to error line on click in output."""
        try:
            idx = self.output_text.index(f"@{event.x},{event.y}")
            line_text = self.output_text.get(f"{idx} linestart", f"{idx} lineend")
            m = re.search(r'Line\s+(\d+)', line_text)
            if m:
                target_line = int(m.group(1))
                self.code_editor.mark_set('insert', f'{target_line}.0')
                self.code_editor.see(f'{target_line}.0')
                self.code_editor.focus_set()
        except Exception:
            pass

    # -----------------------------------------------------------------------
    # Syntax highlighting
    # -----------------------------------------------------------------------

    def _schedule_highlight(self):
        if self._highlight_job:
            self.root.after_cancel(self._highlight_job)
        self._highlight_job = self.root.after(150, self._highlight_syntax)

    def _highlight_syntax(self):
        self._highlight_job = None
        editor = self.code_editor

        # Remove all syntax tags
        for tag in ('mnemonic', 'register', 'number', 'comment', 'string', 'directive', 'label_def'):
            editor.tag_remove(tag, '1.0', 'end')

        content = editor.get('1.0', 'end-1c')
        lines = content.split('\n')

        for line_idx, line in enumerate(lines, 1):
            if not line.strip():
                continue

            col = 0
            # Find comment
            in_str = False
            str_ch = None
            comment_start = -1
            for i, ch in enumerate(line):
                if in_str:
                    if ch == str_ch:
                        in_str = False
                else:
                    if ch in ('"', "'"):
                        in_str = True
                        str_ch = ch
                    elif ch == ';':
                        comment_start = i
                        break

            if comment_start >= 0:
                editor.tag_add('comment', f'{line_idx}.{comment_start}', f'{line_idx}.end')
                active_line = line[:comment_start]
            else:
                active_line = line

            # Label definition (word followed by colon)
            m = re.match(r'^(\s*)([A-Za-z_]\w*)\s*:', active_line)
            if m:
                start = len(m.group(1))
                end = start + len(m.group(2))
                editor.tag_add('label_def', f'{line_idx}.{start}', f'{line_idx}.{end + 1}')

            # Strings
            for m in re.finditer(r'''(["'])(?:(?!\1).)*\1''', active_line):
                editor.tag_add('string', f'{line_idx}.{m.start()}', f'{line_idx}.{m.end()}')

            # Tokenize for keywords (words not inside strings)
            for m in re.finditer(r'\b([A-Za-z_]\w*)\b', active_line):
                word = m.group(1).upper()
                start = m.start()
                end = m.end()
                # Check we're not inside a string
                in_string = False
                for sm in re.finditer(r'''(["'])(?:(?!\1).)*\1''', active_line):
                    if sm.start() <= start < sm.end():
                        in_string = True
                        break
                if in_string:
                    continue

                if word in MNEMONICS:
                    editor.tag_add('mnemonic', f'{line_idx}.{start}', f'{line_idx}.{end}')
                elif word in REGISTERS:
                    editor.tag_add('register', f'{line_idx}.{start}', f'{line_idx}.{end}')
                elif word in DIRECTIVES:
                    editor.tag_add('directive', f'{line_idx}.{start}', f'{line_idx}.{end}')

            # Numbers
            for m in re.finditer(r'\b(0[xX][0-9a-fA-F]+|0[bB][01]+|\d[0-9a-fA-F]*[hH]|\d+)\b', active_line):
                start = m.start()
                end = m.end()
                in_string = False
                for sm in re.finditer(r'''(["'])(?:(?!\1).)*\1''', active_line):
                    if sm.start() <= start < sm.end():
                        in_string = True
                        break
                if not in_string:
                    editor.tag_add('number', f'{line_idx}.{start}', f'{line_idx}.{end}')

    # -----------------------------------------------------------------------
    # File operations
    # -----------------------------------------------------------------------

    def file_new(self):
        if not self._check_save():
            return
        self.code_editor.delete('1.0', 'end')
        self.current_file = None
        self.modified = False
        self._update_title()
        self.clear_output()

    def file_open(self):
        if not self._check_save():
            return
        path = filedialog.askopenfilename(
            title="Open Assembly File",
            filetypes=[("Assembly files", "*.asm *.s"), ("All files", "*.*")],
        )
        if path:
            self._load_file(path)

    def file_save(self):
        if self.current_file:
            self._save_to_file(self.current_file)
        else:
            self.file_save_as()

    def file_save_as(self):
        path = filedialog.asksaveasfilename(
            title="Save Assembly File",
            defaultextension=".asm",
            filetypes=[("Assembly files", "*.asm"), ("All files", "*.*")],
        )
        if path:
            self._save_to_file(path)

    def file_exit(self):
        if self._check_save():
            self.root.destroy()

    def _load_file(self, path: str):
        try:
            with open(path, 'r', encoding='utf-8') as f:
                content = f.read()
            self.code_editor.delete('1.0', 'end')
            self.code_editor.insert('1.0', content)
            # Remove trailing newline that Text widget adds
            if self.code_editor.get('end-2c', 'end-1c') == '\n':
                pass
            self.current_file = path
            self.modified = False
            self.code_editor.edit_modified(False)
            self.code_editor.edit_reset()
            self._update_title()
            self._highlight_syntax()
            self.line_numbers.redraw()
            self.status_var.set(f"Opened: {os.path.basename(path)}")
        except Exception as e:
            messagebox.showerror("Error", f"Could not open file:\n{e}")

    def _save_to_file(self, path: str):
        try:
            content = self.code_editor.get('1.0', 'end-1c')
            with open(path, 'w', encoding='utf-8') as f:
                f.write(content)
            self.current_file = path
            self.modified = False
            self._update_title()
            self.status_var.set(f"Saved: {os.path.basename(path)}")
        except Exception as e:
            messagebox.showerror("Error", f"Could not save file:\n{e}")

    def _check_save(self) -> bool:
        if self.modified:
            result = messagebox.askyesnocancel(
                "Save Changes",
                "The file has been modified. Save changes?",
            )
            if result is None:
                return False
            if result:
                self.file_save()
        return True

    def _update_title(self):
        name = os.path.basename(self.current_file) if self.current_file else "Untitled"
        mod = " *" if self.modified else ""
        self.root.title(f"{name}{mod} - x86-32 Assembly Editor")

    # -----------------------------------------------------------------------
    # Edit operations
    # -----------------------------------------------------------------------

    def _undo(self):
        try:
            self.code_editor.edit_undo()
            self._on_modified()
        except tk.TclError:
            pass
        return 'break'

    def _redo(self):
        try:
            self.code_editor.edit_redo()
            self._on_modified()
        except tk.TclError:
            pass
        return 'break'

    def _cut(self):
        self.code_editor.event_generate('<<Cut>>')
        return 'break'

    def _copy(self):
        self.code_editor.event_generate('<<Copy>>')
        return 'break'

    def _paste(self):
        self.code_editor.event_generate('<<Paste>>')
        return 'break'

    def _select_all(self):
        self.code_editor.tag_add('sel', '1.0', 'end')
        return 'break'

    def _show_find(self):
        FindReplaceDialog(self.root, self)
        return 'break'

    def _show_replace(self):
        FindReplaceDialog(self.root, self)
        return 'break'

    def _goto_line(self):
        line = simpledialog.askinteger("Go to Line", "Enter line number:",
                                       minvalue=1, parent=self.root)
        if line:
            self.code_editor.mark_set('insert', f'{line}.0')
            self.code_editor.see(f'{line}.0')
        return 'break'

    # -----------------------------------------------------------------------
    # Build operations
    # -----------------------------------------------------------------------

    def assemble(self):
        source = self.code_editor.get('1.0', 'end-1c')
        self.assembler = Assembler()
        result = self.assembler.assemble(source)
        self.last_result = result

        # Clear previous error highlights
        self.code_editor.tag_remove('error_line', '1.0', 'end')
        self.error_lines = []

        # Show output
        self.output_text.config(state='normal')
        self.output_text.delete('1.0', 'end')

        if result.success:
            total = len(result.machine_code)
            self.output_text.insert('end', f"Assembly successful! ({total} bytes generated)\n", 'success')
            self.output_text.insert('end', f"Symbols: {len(result.symbols)}\n\n", 'success')
            self.status_var.set(f"Assembly successful - {total} bytes")
        else:
            self.output_text.insert('end', f"Assembly failed with {len(result.errors)} error(s):\n\n", 'error')
            for err in result.errors:
                self.output_text.insert('end', f"  {err}\n", 'clickable')
                # Highlight error line in editor
                m = re.search(r'Line\s+(\d+)', err)
                if m:
                    ln = int(m.group(1))
                    self.error_lines.append(ln)
                    self.code_editor.tag_add('error_line', f'{ln}.0', f'{ln}.end')
            self.status_var.set(f"Assembly failed - {len(result.errors)} error(s)")

        self.output_text.config(state='disabled')

    def show_listing(self):
        if not self.last_result:
            self.assemble()
        if self.last_result:
            self.output_text.config(state='normal')
            self.output_text.delete('1.0', 'end')
            self.output_text.insert('end', self.last_result.listing)
            self.output_text.config(state='disabled')
            self.output_text.see('1.0')

    def clear_output(self):
        self.output_text.config(state='normal')
        self.output_text.delete('1.0', 'end')
        self.output_text.config(state='disabled')
        self.code_editor.tag_remove('error_line', '1.0', 'end')
        self.status_var.set("Ready")

    def export_listing(self):
        if not self.last_result:
            self.assemble()
        if not self.last_result:
            return
        path = filedialog.asksaveasfilename(
            title="Export Listing",
            defaultextension=".lst",
            filetypes=[("Listing files", "*.lst"), ("Text files", "*.txt"), ("All files", "*.*")],
        )
        if path:
            try:
                with open(path, 'w', encoding='utf-8') as f:
                    f.write(self.last_result.listing)
                self.status_var.set(f"Listing exported to {os.path.basename(path)}")
            except Exception as e:
                messagebox.showerror("Error", f"Could not export listing:\n{e}")

    def export_binary(self):
        if not self.last_result:
            self.assemble()
        if not self.last_result or not self.last_result.success:
            messagebox.showwarning("Warning", "Cannot export binary: assembly has errors")
            return
        path = filedialog.asksaveasfilename(
            title="Export Binary",
            defaultextension=".bin",
            filetypes=[("Binary files", "*.bin"), ("All files", "*.*")],
        )
        if path:
            try:
                with open(path, 'wb') as f:
                    f.write(self.last_result.machine_code)
                self.status_var.set(f"Binary exported to {os.path.basename(path)} ({len(self.last_result.machine_code)} bytes)")
            except Exception as e:
                messagebox.showerror("Error", f"Could not export binary:\n{e}")

    # -----------------------------------------------------------------------
    # Help dialogs
    # -----------------------------------------------------------------------

    def _show_help(self):
        help_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'help.txt')
        try:
            with open(help_path, 'r', encoding='utf-8') as f:
                help_text = f.read()
        except FileNotFoundError:
            help_text = "Help file not found."

        win = tk.Toplevel(self.root)
        win.title("Help")
        win.geometry("650x500")
        text = tk.Text(win, wrap='word', font=('Consolas', 11), padx=10, pady=10)
        scroll = ttk.Scrollbar(win, orient='vertical', command=text.yview)
        text.config(yscrollcommand=scroll.set)
        scroll.pack(side='right', fill='y')
        text.pack(fill='both', expand=True)
        text.insert('1.0', help_text)
        text.config(state='disabled')

    def _show_instruction_ref(self):
        ref = """x86-32 Instruction Reference
============================

DATA MOVEMENT:
  MOV dst, src     - Move data
  XCHG dst, src    - Exchange operands
  LEA dst, [mem]   - Load effective address
  MOVZX dst, src   - Move with zero-extend
  MOVSX dst, src   - Move with sign-extend
  PUSH src         - Push onto stack
  POP dst          - Pop from stack

ARITHMETIC:
  ADD dst, src     - Add
  SUB dst, src     - Subtract
  ADC dst, src     - Add with carry
  SBB dst, src     - Subtract with borrow
  INC dst          - Increment by 1
  DEC dst          - Decrement by 1
  MUL src          - Unsigned multiply (EDX:EAX)
  IMUL src/dst,src - Signed multiply
  DIV src          - Unsigned divide
  IDIV src         - Signed divide
  NEG dst          - Two's complement negate

LOGICAL:
  AND dst, src     - Bitwise AND
  OR dst, src      - Bitwise OR
  XOR dst, src     - Bitwise XOR
  NOT dst          - Bitwise NOT
  TEST dst, src    - Bitwise AND (flags only)

SHIFT/ROTATE:
  SHL dst, count   - Shift left
  SHR dst, count   - Shift right (logical)
  SAR dst, count   - Shift right (arithmetic)
  ROL dst, count   - Rotate left
  ROR dst, count   - Rotate right

CONTROL FLOW:
  JMP target       - Unconditional jump
  Jcc target       - Conditional jump (JE,JNE,JG,JL,etc.)
  CALL target      - Call subroutine
  RET              - Return from subroutine
  LOOP target      - Decrement ECX and jump if not zero
  INT n            - Software interrupt

STRING:
  MOVSB/MOVSD      - Move string byte/dword
  STOSB/STOSD      - Store string byte/dword
  LODSB/LODSD      - Load string byte/dword
  REP prefix       - Repeat string operation

FLAGS:
  CLC/STC          - Clear/Set carry flag
  CLD/STD          - Clear/Set direction flag
  CLI/STI          - Clear/Set interrupt flag

DATA DIRECTIVES:
  DB values        - Define byte(s)
  DW values        - Define word(s)
  DD values        - Define dword(s)
  EQU value        - Define constant
  TIMES n instr    - Repeat instruction n times

ADDRESSING MODES:
  MOV EAX, EBX           - Register
  MOV EAX, 42            - Immediate
  MOV EAX, [label]       - Direct memory
  MOV EAX, [EBX]         - Register indirect
  MOV EAX, [EBP+8]       - Base + displacement
  MOV EAX, [EBX+ECX*4+8] - Base + index*scale + disp
"""
        win = tk.Toplevel(self.root)
        win.title("Instruction Reference")
        win.geometry("600x600")
        text = tk.Text(win, wrap='word', font=('Consolas', 11), padx=10, pady=10)
        scroll = ttk.Scrollbar(win, orient='vertical', command=text.yview)
        text.config(yscrollcommand=scroll.set)
        scroll.pack(side='right', fill='y')
        text.pack(fill='both', expand=True)
        text.insert('1.0', ref)
        text.config(state='disabled')

    def _show_about(self):
        messagebox.showinfo(
            "About",
            "x86-32 Assembly Code Editor\n\n"
            "Two-pass assembler with NASM-like syntax.\n"
            "Supports 50+ mnemonics, all 6 addressing modes,\n"
            "ModR/M + SIB byte encoding, syntax highlighting,\n"
            "and formatted listing generation.\n\n"
            "System Programming Project 1\n"
            "OS and System Programming Course",
        )

    # -----------------------------------------------------------------------
    # Load file from command line
    # -----------------------------------------------------------------------

    def open_file_from_arg(self, path: str):
        if os.path.isfile(path):
            self._load_file(path)
