namespace Fable.Import

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser 

module monaco =
    type IDisposable =
        abstract dispose: unit -> unit

    and IEvent<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: listener: Func<'T, obj> * ?thisArg: obj -> IDisposable

    and [<Import("Emitter","monaco")>] Emitter<'T>() =
        member __.``event`` with get(): IEvent<'T> = failwith "JS only" and set(v: IEvent<'T>): unit = failwith "JS only"
        member __.fire(?``event``: 'T): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"

    and Severity =
        | Ignore = 0
        | Info = 1
        | Warning = 2
        | Error = 3

    and TValueCallback<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: value: 'T -> unit

    and ProgressCallback =
        [<Emit("$0($1...)")>] abstract Invoke: progress: obj -> obj

    and [<Import("CancellationTokenSource","monaco")>] CancellationTokenSource() =
        member __.token with get(): CancellationToken = failwith "JS only" and set(v: CancellationToken): unit = failwith "JS only"
        member __.cancel(): unit = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"

    and CancellationToken =
        abstract isCancellationRequested: bool with get, set
        abstract onCancellationRequested: IEvent<obj> with get, set

    and [<Import("Uri","monaco")>] Uri() =
        member __.scheme with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.authority with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.path with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.query with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fragment with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.fsPath with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``with``(change: obj): Uri = failwith "JS only"
        static member parse(value: string): Uri = failwith "JS only"
        static member file(path: string): Uri = failwith "JS only"
        static member from(components: obj): Uri = failwith "JS only"
        member __.toString(?skipEncoding: bool): string = failwith "JS only"
        member __.toJSON(): obj = failwith "JS only"
        static member revive(data: obj): Uri = failwith "JS only"

    and KeyCode =
        | Unknown = 0
        | Backspace = 1
        | Tab = 2
        | Enter = 3
        | Shift = 4
        | Ctrl = 5
        | Alt = 6
        | PauseBreak = 7
        | CapsLock = 8
        | Escape = 9
        | Space = 10
        | PageUp = 11
        | PageDown = 12
        | End = 13
        | Home = 14
        | LeftArrow = 15
        | UpArrow = 16
        | RightArrow = 17
        | DownArrow = 18
        | Insert = 19
        | Delete = 20
        | KEY_0 = 21
        | KEY_1 = 22
        | KEY_2 = 23
        | KEY_3 = 24
        | KEY_4 = 25
        | KEY_5 = 26
        | KEY_6 = 27
        | KEY_7 = 28
        | KEY_8 = 29
        | KEY_9 = 30
        | KEY_A = 31
        | KEY_B = 32
        | KEY_C = 33
        | KEY_D = 34
        | KEY_E = 35
        | KEY_F = 36
        | KEY_G = 37
        | KEY_H = 38
        | KEY_I = 39
        | KEY_J = 40
        | KEY_K = 41
        | KEY_L = 42
        | KEY_M = 43
        | KEY_N = 44
        | KEY_O = 45
        | KEY_P = 46
        | KEY_Q = 47
        | KEY_R = 48
        | KEY_S = 49
        | KEY_T = 50
        | KEY_U = 51
        | KEY_V = 52
        | KEY_W = 53
        | KEY_X = 54
        | KEY_Y = 55
        | KEY_Z = 56
        | Meta = 57
        | ContextMenu = 58
        | F1 = 59
        | F2 = 60
        | F3 = 61
        | F4 = 62
        | F5 = 63
        | F6 = 64
        | F7 = 65
        | F8 = 66
        | F9 = 67
        | F10 = 68
        | F11 = 69
        | F12 = 70
        | F13 = 71
        | F14 = 72
        | F15 = 73
        | F16 = 74
        | F17 = 75
        | F18 = 76
        | F19 = 77
        | NumLock = 78
        | ScrollLock = 79
        | US_SEMICOLON = 80
        | US_EQUAL = 81
        | US_COMMA = 82
        | US_MINUS = 83
        | US_DOT = 84
        | US_SLASH = 85
        | US_BACKTICK = 86
        | US_OPEN_SQUARE_BRACKET = 87
        | US_BACKSLASH = 88
        | US_CLOSE_SQUARE_BRACKET = 89
        | US_QUOTE = 90
        | OEM_8 = 91
        | OEM_102 = 92
        | NUMPAD_0 = 93
        | NUMPAD_1 = 94
        | NUMPAD_2 = 95
        | NUMPAD_3 = 96
        | NUMPAD_4 = 97
        | NUMPAD_5 = 98
        | NUMPAD_6 = 99
        | NUMPAD_7 = 100
        | NUMPAD_8 = 101
        | NUMPAD_9 = 102
        | NUMPAD_MULTIPLY = 103
        | NUMPAD_ADD = 104
        | NUMPAD_SEPARATOR = 105
        | NUMPAD_SUBTRACT = 106
        | NUMPAD_DECIMAL = 107
        | NUMPAD_DIVIDE = 108
        | MAX_VALUE = 109

    and [<Import("KeyMod","monaco")>] KeyMod() =
        member __.CtrlCmd with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Shift with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Alt with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.WinCtrl with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member chord(firstPart: float, secondPart: float): float = failwith "JS only"

    and IHTMLContentElementCode =
        abstract language: string with get, set
        abstract value: string with get, set

    and IHTMLContentElement =
        abstract formattedText: string option with get, set
        abstract text: string option with get, set
        abstract className: string option with get, set
        abstract style: string option with get, set
        abstract customStyle: obj option with get, set
        abstract tagName: string option with get, set
        abstract children: ResizeArray<IHTMLContentElement> option with get, set
        abstract isText: bool option with get, set
        abstract role: string option with get, set
        abstract markdown: string option with get, set
        abstract code: IHTMLContentElementCode option with get, set

    and IKeyboardEvent =
        abstract browserEvent: Event with get, set
        abstract target: HTMLElement with get, set
        abstract ctrlKey: bool with get, set
        abstract shiftKey: bool with get, set
        abstract altKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract keyCode: KeyCode with get, set
        abstract clone: unit -> IKeyboardEvent
        abstract asKeybinding: unit -> float
        abstract equals: keybinding: float -> bool
        abstract preventDefault: unit -> unit
        abstract stopPropagation: unit -> unit

    and IMouseEvent =
        abstract browserEvent: MouseEvent with get, set
        abstract leftButton: bool with get, set
        abstract middleButton: bool with get, set
        abstract rightButton: bool with get, set
        abstract target: HTMLElement with get, set
        abstract detail: float with get, set
        abstract posx: float with get, set
        abstract posy: float with get, set
        abstract ctrlKey: bool with get, set
        abstract shiftKey: bool with get, set
        abstract altKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract timestamp: float with get, set
        abstract preventDefault: unit -> unit
        abstract stopPropagation: unit -> unit

    and IScrollEvent =
        abstract scrollTop: float with get, set
        abstract scrollLeft: float with get, set
        abstract scrollWidth: float with get, set
        abstract scrollHeight: float with get, set
        abstract scrollTopChanged: bool with get, set
        abstract scrollLeftChanged: bool with get, set
        abstract scrollWidthChanged: bool with get, set
        abstract scrollHeightChanged: bool with get, set

    and IPosition =
        abstract lineNumber: float with get, set
        abstract column: float with get, set

    and IRange =
        abstract startLineNumber: float with get, set
        abstract startColumn: float with get, set
        abstract endLineNumber: float with get, set
        abstract endColumn: float with get, set

    and SelectionDirection =
        | LTR = 0
        | RTL = 1

    and ISelection =
        abstract selectionStartLineNumber: float with get, set
        abstract selectionStartColumn: float with get, set
        abstract positionLineNumber: float with get, set
        abstract positionColumn: float with get, set

    and [<Import("Position","monaco")>] Position(lineNumber: float, column: float) =
        member __.lineNumber with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.column with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.equals(other: IPosition): bool = failwith "JS only"
        static member equals(a: IPosition, b: IPosition): bool = failwith "JS only"
        member __.isBefore(other: IPosition): bool = failwith "JS only"
        static member isBefore(a: IPosition, b: IPosition): bool = failwith "JS only"
        member __.isBeforeOrEqual(other: IPosition): bool = failwith "JS only"
        static member isBeforeOrEqual(a: IPosition, b: IPosition): bool = failwith "JS only"
        member __.clone(): Position = failwith "JS only"
        member __.toString(): string = failwith "JS only"
        static member lift(pos: IPosition): Position = failwith "JS only"
        static member isIPosition(obj: obj): obj = failwith "JS only"
        static member asEmptyRange(position: IPosition): IRange = failwith "JS only"
        static member startPosition(range: IRange): IPosition = failwith "JS only"
        static member endPosition(range: IRange): IPosition = failwith "JS only"

    and [<Import("Range","monaco")>] Range(startLineNumber: float, startColumn: float, endLineNumber: float, endColumn: float) =
        member __.startLineNumber with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.startColumn with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.endLineNumber with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.endColumn with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.isEmpty(): bool = failwith "JS only"
        member __.containsPosition(position: IPosition): bool = failwith "JS only"
        member __.containsRange(range: IRange): bool = failwith "JS only"
        member __.plusRange(range: IRange): Range = failwith "JS only"
        member __.intersectRanges(range: IRange): Range = failwith "JS only"
        member __.equalsRange(other: IRange): bool = failwith "JS only"
        member __.getEndPosition(): Position = failwith "JS only"
        member __.getStartPosition(): Position = failwith "JS only"
        member __.cloneRange(): Range = failwith "JS only"
        member __.toString(): string = failwith "JS only"
        member __.setEndPosition(endLineNumber: float, endColumn: float): Range = failwith "JS only"
        member __.setStartPosition(startLineNumber: float, startColumn: float): Range = failwith "JS only"
        member __.collapseToStart(): Range = failwith "JS only"
        static member lift(range: IRange): Range = failwith "JS only"
        static member isIRange(obj: obj): obj = failwith "JS only"
        static member isEmpty(range: IRange): bool = failwith "JS only"
        static member containsPosition(range: IRange, position: IPosition): bool = failwith "JS only"
        static member containsRange(range: IRange, otherRange: IRange): bool = failwith "JS only"
        static member areIntersectingOrTouching(a: IRange, b: IRange): bool = failwith "JS only"
        static member intersectRanges(a: IRange, b: IRange): Range = failwith "JS only"
        static member plusRange(a: IRange, b: IRange): Range = failwith "JS only"
        static member equalsRange(a: IRange, b: IRange): bool = failwith "JS only"
        static member compareRangesUsingStarts(a: IRange, b: IRange): float = failwith "JS only"
        static member compareRangesUsingEnds(a: IRange, b: IRange): float = failwith "JS only"
        static member spansMultipleLines(range: IRange): bool = failwith "JS only"
        static member collapseToStart(range: IRange): IRange = failwith "JS only"

    and [<Import("Selection","monaco")>] Selection(selectionStartLineNumber: float, selectionStartColumn: float, positionLineNumber: float, positionColumn: float) =
        inherit Range(selectionStartLineNumber, selectionStartColumn, positionLineNumber, positionColumn)
        member __.selectionStartLineNumber with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.selectionStartColumn with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.positionLineNumber with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.positionColumn with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.clone(): Selection = failwith "JS only"
        member __.toString(): string = failwith "JS only"
        member __.equalsSelection(other: ISelection): bool = failwith "JS only"
        member __.getDirection(): SelectionDirection = failwith "JS only"
        member __.setEndPosition(endLineNumber: float, endColumn: float): Selection = failwith "JS only"
        member __.setStartPosition(startLineNumber: float, startColumn: float): Selection = failwith "JS only"
        static member createSelection(selectionStartLineNumber: float, selectionStartColumn: float, positionLineNumber: float, positionColumn: float): Selection = failwith "JS only"
        static member liftSelection(sel: ISelection): Selection = failwith "JS only"
        static member selectionsEqual(a: ISelection, b: ISelection): bool = failwith "JS only"
        static member selectionsArrEqual(a: ResizeArray<ISelection>, b: ResizeArray<ISelection>): bool = failwith "JS only"
        static member isISelection(obj: obj): bool = failwith "JS only"
        static member createWithDirection(startLineNumber: float, startColumn: float, endLineNumber: float, endColumn: float, direction: SelectionDirection): Selection = failwith "JS only"

    module editor =
        type [<Import("editor.MonacoWebWorker","monaco")>] MonacoWebWorker<'T>() =
            member __.dispose(): unit = failwith "JS only"
            member __.getProxy(): Promise<'T> = failwith "JS only"
            member __.withSyncedResources(resources: ResizeArray<Uri>): Promise<unit> = failwith "JS only"

        and IWebWorkerOptions =
            abstract moduleId: string with get, set

        and IEditorConstructionOptions =
            inherit ICodeEditorWidgetCreationOptions
            abstract value: string option with get, set
            abstract language: string option with get, set

        and IDiffEditorConstructionOptions =
            inherit IDiffEditorOptions


        and IEditorOverrideServices =
            interface end

        and IMarkerData =
            abstract code: string option with get, set
            abstract severity: Severity with get, set
            abstract message: string with get, set
            abstract source: string option with get, set
            abstract startLineNumber: float with get, set
            abstract startColumn: float with get, set
            abstract endLineNumber: float with get, set
            abstract endColumn: float with get, set

        and IColorizerOptions =
            abstract tabSize: float option with get, set

        and IColorizerElementOptions =
            inherit IColorizerOptions
            abstract theme: string option with get, set
            abstract mimeType: string option with get, set

        and ScrollbarVisibility =
            | Auto = 1
            | Hidden = 2
            | Visible = 3

        and IAction =
            inherit IDisposable
            abstract id: string with get, set
            abstract label: string with get, set
            abstract tooltip: string with get, set
            abstract ``class``: string with get, set
            abstract enabled: bool with get, set
            abstract ``checked``: bool with get, set
            abstract run: ?``event``: obj -> Promise<obj>

        and IEditorScrollbarOptions =
            abstract arrowSize: float option with get, set
            abstract vertical: string option with get, set
            abstract horizontal: string option with get, set
            abstract useShadows: bool option with get, set
            abstract verticalHasArrows: bool option with get, set
            abstract horizontalHasArrows: bool option with get, set
            abstract handleMouseWheel: bool option with get, set
            abstract horizontalScrollbarSize: float option with get, set
            abstract verticalScrollbarSize: float option with get, set
            abstract verticalSliderSize: float option with get, set
            abstract horizontalSliderSize: float option with get, set

        and WrappingIndent =
            | None = 0
            | Same = 1
            | Indent = 2

        and IEditorOptions =
            abstract experimentalScreenReader: bool option with get, set
            abstract ariaLabel: string option with get, set
            abstract rulers: ResizeArray<float> option with get, set
            abstract wordSeparators: string option with get, set
            abstract selectionClipboard: bool option with get, set
            abstract lineNumbers: obj option with get, set
            abstract selectOnLineNumbers: bool option with get, set
            abstract lineNumbersMinChars: float option with get, set
            abstract glyphMargin: bool option with get, set
            abstract lineDecorationsWidth: float option with get, set
            abstract revealHorizontalRightPadding: float option with get, set
            abstract roundedSelection: bool option with get, set
            abstract theme: string option with get, set
            abstract readOnly: bool option with get, set
            abstract scrollbar: IEditorScrollbarOptions option with get, set
            abstract overviewRulerLanes: float option with get, set
            abstract cursorBlinking: string option with get, set
            abstract cursorStyle: string option with get, set
            abstract fontLigatures: bool option with get, set
            abstract disableTranslate3d: bool option with get, set
            abstract hideCursorInOverviewRuler: bool option with get, set
            abstract scrollBeyondLastLine: bool option with get, set
            abstract automaticLayout: bool option with get, set
            abstract wrappingColumn: float option with get, set
            abstract wrappingIndent: string option with get, set
            abstract wordWrapBreakBeforeCharacters: string option with get, set
            abstract wordWrapBreakAfterCharacters: string option with get, set
            abstract wordWrapBreakObtrusiveCharacters: string option with get, set
            abstract tabFocusMode: bool option with get, set
            abstract stopRenderingLineAfter: float option with get, set
            abstract hover: bool option with get, set
            abstract contextmenu: bool option with get, set
            abstract mouseWheelScrollSensitivity: float option with get, set
            abstract quickSuggestions: bool option with get, set
            abstract quickSuggestionsDelay: float option with get, set
            abstract parameterHints: bool option with get, set
            abstract iconsInSuggestions: bool option with get, set
            abstract autoClosingBrackets: bool option with get, set
            abstract formatOnType: bool option with get, set
            abstract suggestOnTriggerCharacters: bool option with get, set
            abstract acceptSuggestionOnEnter: bool option with get, set
            abstract selectionHighlight: bool option with get, set
            abstract outlineMarkers: bool option with get, set
            abstract referenceInfos: bool option with get, set
            abstract folding: bool option with get, set
            abstract renderWhitespace: bool option with get, set
            abstract indentGuides: bool option with get, set
            abstract useTabStops: bool option with get, set
            abstract fontFamily: string option with get, set
            abstract fontSize: float option with get, set
            abstract lineHeight: float option with get, set

        and IDiffEditorOptions =
            inherit IEditorOptions
            abstract enableSplitViewResizing: bool option with get, set
            abstract renderSideBySide: bool option with get, set
            abstract ignoreTrimWhitespace: bool option with get, set
            abstract originalEditable: bool option with get, set

        and [<Import("editor.InternalEditorScrollbarOptions","monaco")>] InternalEditorScrollbarOptions() =
            member __._internalEditorScrollbarOptionsBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.arrowSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.vertical with get(): ScrollbarVisibility = failwith "JS only" and set(v: ScrollbarVisibility): unit = failwith "JS only"
            member __.horizontal with get(): ScrollbarVisibility = failwith "JS only" and set(v: ScrollbarVisibility): unit = failwith "JS only"
            member __.useShadows with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.verticalHasArrows with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.horizontalHasArrows with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.handleMouseWheel with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.horizontalScrollbarSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.horizontalSliderSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.verticalScrollbarSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.verticalSliderSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.mouseWheelScrollSensitivity with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("editor.EditorWrappingInfo","monaco")>] EditorWrappingInfo() =
            member __._editorWrappingInfoBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.isViewportWrapping with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.wrappingColumn with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.wrappingIndent with get(): WrappingIndent = failwith "JS only" and set(v: WrappingIndent): unit = failwith "JS only"
            member __.wordWrapBreakBeforeCharacters with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.wordWrapBreakAfterCharacters with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.wordWrapBreakObtrusiveCharacters with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"

        and [<Import("editor.InternalEditorViewOptions","monaco")>] InternalEditorViewOptions() =
            member __._internalEditorViewOptionsBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.theme with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.canUseTranslate3d with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.experimentalScreenReader with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.rulers with get(): ResizeArray<float> = failwith "JS only" and set(v: ResizeArray<float>): unit = failwith "JS only"
            member __.ariaLabel with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.lineNumbers with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.selectOnLineNumbers with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.glyphMargin with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.revealHorizontalRightPadding with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.roundedSelection with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.overviewRulerLanes with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.cursorBlinking with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.cursorStyle with get(): TextEditorCursorStyle = failwith "JS only" and set(v: TextEditorCursorStyle): unit = failwith "JS only"
            member __.hideCursorInOverviewRuler with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.scrollBeyondLastLine with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.editorClassName with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.stopRenderingLineAfter with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.renderWhitespace with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.indentGuides with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.scrollbar with get(): InternalEditorScrollbarOptions = failwith "JS only" and set(v: InternalEditorScrollbarOptions): unit = failwith "JS only"

        and IViewConfigurationChangedEvent =
            abstract theme: bool with get, set
            abstract canUseTranslate3d: bool with get, set
            abstract experimentalScreenReader: bool with get, set
            abstract rulers: bool with get, set
            abstract ariaLabel: bool with get, set
            abstract lineNumbers: bool with get, set
            abstract selectOnLineNumbers: bool with get, set
            abstract glyphMargin: bool with get, set
            abstract revealHorizontalRightPadding: bool with get, set
            abstract roundedSelection: bool with get, set
            abstract overviewRulerLanes: bool with get, set
            abstract cursorBlinking: bool with get, set
            abstract cursorStyle: bool with get, set
            abstract hideCursorInOverviewRuler: bool with get, set
            abstract scrollBeyondLastLine: bool with get, set
            abstract editorClassName: bool with get, set
            abstract stopRenderingLineAfter: bool with get, set
            abstract renderWhitespace: bool with get, set
            abstract indentGuides: bool with get, set
            abstract scrollbar: bool with get, set

        and [<Import("editor.EditorContribOptions","monaco")>] EditorContribOptions() =
            member __.selectionClipboard with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.hover with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.contextmenu with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.quickSuggestions with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.quickSuggestionsDelay with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.parameterHints with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.iconsInSuggestions with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.formatOnType with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.suggestOnTriggerCharacters with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.acceptSuggestionOnEnter with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.selectionHighlight with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.outlineMarkers with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.referenceInfos with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.folding with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"

        and [<Import("editor.InternalEditorOptions","monaco")>] InternalEditorOptions() =
            member __._internalEditorOptionsBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.lineHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.readOnly with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.wordSeparators with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.autoClosingBrackets with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.useTabStops with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.tabFocusMode with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.layoutInfo with get(): EditorLayoutInfo = failwith "JS only" and set(v: EditorLayoutInfo): unit = failwith "JS only"
            member __.fontInfo with get(): FontInfo = failwith "JS only" and set(v: FontInfo): unit = failwith "JS only"
            member __.viewInfo with get(): InternalEditorViewOptions = failwith "JS only" and set(v: InternalEditorViewOptions): unit = failwith "JS only"
            member __.wrappingInfo with get(): EditorWrappingInfo = failwith "JS only" and set(v: EditorWrappingInfo): unit = failwith "JS only"
            member __.contribInfo with get(): EditorContribOptions = failwith "JS only" and set(v: EditorContribOptions): unit = failwith "JS only"

        and IConfigurationChangedEvent =
            abstract lineHeight: bool with get, set
            abstract readOnly: bool with get, set
            abstract wordSeparators: bool with get, set
            abstract autoClosingBrackets: bool with get, set
            abstract useTabStops: bool with get, set
            abstract tabFocusMode: bool with get, set
            abstract layoutInfo: bool with get, set
            abstract fontInfo: bool with get, set
            abstract viewInfo: IViewConfigurationChangedEvent with get, set
            abstract wrappingInfo: bool with get, set
            abstract contribInfo: bool with get, set

        and OverviewRulerLane =
            | Left = 1
            | Center = 2
            | Right = 4
            | Full = 7

        and IModelDecorationOverviewRulerOptions =
            abstract color: string with get, set
            abstract darkColor: string with get, set
            abstract position: OverviewRulerLane with get, set

        and IModelDecorationOptions =
            abstract stickiness: TrackedRangeStickiness option with get, set
            abstract className: string option with get, set
            abstract hoverMessage: string option with get, set
            abstract htmlMessage: ResizeArray<IHTMLContentElement> option with get, set
            abstract isWholeLine: bool option with get, set
            abstract showInOverviewRuler: string option with get, set
            abstract overviewRuler: IModelDecorationOverviewRulerOptions option with get, set
            abstract glyphMarginClassName: string option with get, set
            abstract linesDecorationsClassName: string option with get, set
            abstract inlineClassName: string option with get, set

        and IModelDeltaDecoration =
            abstract range: IRange with get, set
            abstract options: IModelDecorationOptions with get, set

        and IModelDecoration =
            abstract id: string with get, set
            abstract ownerId: float with get, set
            abstract range: Range with get, set
            abstract options: IModelDecorationOptions with get, set

        and IWordAtPosition =
            abstract word: string with get, set
            abstract startColumn: float with get, set
            abstract endColumn: float with get, set

        and EndOfLinePreference =
            | TextDefined = 0
            | LF = 1
            | CRLF = 2

        and DefaultEndOfLine =
            | LF = 1
            | CRLF = 2

        and EndOfLineSequence =
            | LF = 0
            | CRLF = 1

        and ISingleEditOperationIdentifier =
            abstract major: float with get, set
            abstract minor: float with get, set

        and IEditOperationBuilder =
            abstract addEditOperation: range: Range * text: string -> unit
            abstract trackSelection: selection: Selection * ?trackPreviousOnEmpty: bool -> string

        and ICursorStateComputerData =
            abstract getInverseEditOperations: unit -> ResizeArray<IIdentifiedSingleEditOperation>
            abstract getTrackedSelection: id: string -> Selection

        and ICommand =
            abstract getEditOperations: model: ITokenizedModel * builder: IEditOperationBuilder -> unit
            abstract computeCursorState: model: ITokenizedModel * helper: ICursorStateComputerData -> Selection

        and ISingleEditOperation =
            abstract range: IRange with get, set
            abstract text: string with get, set
            abstract forceMoveMarkers: bool option with get, set

        and IIdentifiedSingleEditOperation =
            abstract identifier: ISingleEditOperationIdentifier with get, set
            abstract range: Range with get, set
            abstract text: string with get, set
            abstract forceMoveMarkers: bool with get, set
            abstract isAutoWhitespaceEdit: bool option with get, set

        and ICursorStateComputer =
            [<Emit("$0($1...)")>] abstract Invoke: inverseEditOperations: ResizeArray<IIdentifiedSingleEditOperation> -> ResizeArray<Selection>

        and ITextModelResolvedOptions =
            abstract tabSize: float with get, set
            abstract insertSpaces: bool with get, set
            abstract defaultEOL: DefaultEndOfLine with get, set
            abstract trimAutoWhitespace: bool with get, set

        and ITextModelUpdateOptions =
            abstract tabSize: float option with get, set
            abstract insertSpaces: bool option with get, set
            abstract trimAutoWhitespace: bool option with get, set

        and IModelOptionsChangedEvent =
            abstract tabSize: bool with get, set
            abstract insertSpaces: bool with get, set
            abstract trimAutoWhitespace: bool with get, set

        and ITextModel =
            abstract getOptions: unit -> ITextModelResolvedOptions
            abstract getVersionId: unit -> float
            abstract getAlternativeVersionId: unit -> float
            abstract setValue: newValue: string -> unit
            abstract setValueFromRawText: newValue: IRawText -> unit
            abstract getValue: ?eol: EndOfLinePreference * ?preserveBOM: bool -> string
            abstract getValueLength: ?eol: EndOfLinePreference * ?preserveBOM: bool -> float
            abstract toRawText: unit -> IRawText
            abstract equals: other: IRawText -> bool
            abstract getValueInRange: range: IRange * ?eol: EndOfLinePreference -> string
            abstract getValueLengthInRange: range: IRange -> float
            abstract getLineCount: unit -> float
            abstract getLineContent: lineNumber: float -> string
            abstract getLinesContent: unit -> ResizeArray<string>
            abstract getEOL: unit -> string
            abstract setEOL: eol: EndOfLineSequence -> unit
            abstract getLineMinColumn: lineNumber: float -> float
            abstract getLineMaxColumn: lineNumber: float -> float
            abstract getLineFirstNonWhitespaceColumn: lineNumber: float -> float
            abstract getLineLastNonWhitespaceColumn: lineNumber: float -> float
            abstract validatePosition: position: IPosition -> Position
            abstract modifyPosition: position: IPosition * offset: float -> Position
            abstract validateRange: range: IRange -> Range
            abstract getOffsetAt: position: IPosition -> float
            abstract getPositionAt: offset: float -> Position
            abstract getFullModelRange: unit -> Range
            abstract isDisposed: unit -> bool

        and IReadOnlyModel =
            inherit ITextModel
            abstract uri: Uri with get, set
            abstract getModeId: unit -> string
            abstract getWordAtPosition: position: IPosition -> IWordAtPosition
            abstract getWordUntilPosition: position: IPosition -> IWordAtPosition

        and ITokenizedModel =
            inherit ITextModel
           // abstract getMode: unit -> languages.IMode
           // abstract setMode: newMode: U2<languages.IMode, Promise<languages.IMode>> -> 
            abstract getMode: unit -> obj
            abstract setMode: newMode: U2<obj, Promise<obj>> -> unit
            abstract getWordAtPosition: position: IPosition -> IWordAtPosition
            abstract getWordUntilPosition: position: IPosition -> IWordAtPosition

        and ITextModelWithMarkers =
            inherit ITextModel


        and TrackedRangeStickiness =
            | AlwaysGrowsWhenTypingAtEdges = 0
            | NeverGrowsWhenTypingAtEdges = 1
            | GrowsOnlyWhenTypingBefore = 2
            | GrowsOnlyWhenTypingAfter = 3

        and ITextModelWithTrackedRanges =
            inherit ITextModel


        and ITextModelWithDecorations =
            abstract deltaDecorations: oldDecorations: ResizeArray<string> * newDecorations: ResizeArray<IModelDeltaDecoration> * ?ownerId: float -> ResizeArray<string>
            abstract getDecorationOptions: id: string -> IModelDecorationOptions
            abstract getDecorationRange: id: string -> Range
            abstract getLineDecorations: lineNumber: float * ?ownerId: float * ?filterOutValidation: bool -> ResizeArray<IModelDecoration>
            abstract getLinesDecorations: startLineNumber: float * endLineNumber: float * ?ownerId: float * ?filterOutValidation: bool -> ResizeArray<IModelDecoration>
            abstract getDecorationsInRange: range: IRange * ?ownerId: float * ?filterOutValidation: bool -> ResizeArray<IModelDecoration>
            abstract getAllDecorations: ?ownerId: float * ?filterOutValidation: bool -> ResizeArray<IModelDecoration>

        and IEditableTextModel =
            inherit ITextModelWithMarkers
            abstract normalizeIndentation: str: string -> string
            abstract getOneIndent: unit -> string
            abstract updateOptions: newOpts: ITextModelUpdateOptions -> unit
            abstract detectIndentation: defaultInsertSpaces: bool * defaultTabSize: float -> unit
            abstract pushStackElement: unit -> unit
            abstract pushEditOperations: beforeCursorState: ResizeArray<Selection> * editOperations: ResizeArray<IIdentifiedSingleEditOperation> * cursorStateComputer: ICursorStateComputer -> ResizeArray<Selection>
            abstract applyEdits: operations: ResizeArray<IIdentifiedSingleEditOperation> -> ResizeArray<IIdentifiedSingleEditOperation>

        and IModel =
            inherit IReadOnlyModel
            inherit IEditableTextModel
            inherit ITextModelWithMarkers
            inherit ITokenizedModel
            inherit ITextModelWithTrackedRanges
            inherit ITextModelWithDecorations
            inherit IEditorModel
            abstract id: string with get, set
            abstract onDidChangeRawContent: listener: Func<IModelContentChangedEvent, unit> -> IDisposable
            abstract onDidChangeContent: listener: Func<IModelContentChangedEvent2, unit> -> IDisposable
            abstract onDidChangeDecorations: listener: Func<IModelDecorationsChangedEvent, unit> -> IDisposable
            abstract onDidChangeOptions: listener: Func<IModelOptionsChangedEvent, unit> -> IDisposable
            abstract onDidChangeMode: listener: Func<IModelModeChangedEvent, unit> -> IDisposable
            abstract onWillDispose: listener: Func<unit> -> IDisposable
            abstract dispose: unit -> unit
            abstract findMatches: searchString: string * searchOnlyEditableRange: bool * isRegex: bool * matchCase: bool * wholeWord: bool * ?limitResultCount: float -> ResizeArray<Range>
            abstract findMatches: searchString: string * searchScope: IRange * isRegex: bool * matchCase: bool * wholeWord: bool * ?limitResultCount: float -> ResizeArray<Range>
            abstract findNextMatch: searchString: string * searchStart: IPosition * isRegex: bool * matchCase: bool * wholeWord: bool -> Range
            abstract findPreviousMatch: searchString: string * searchStart: IPosition * isRegex: bool * matchCase: bool * wholeWord: bool -> Range

        and IModelModeChangedEvent =
            // abstract oldMode: languages.IMode with get, set
            // abstract newMode: languages.IMode with get, set
            abstract oldMode: obj with get, set
            abstract newMode: obj with get, set

        and IModelContentChangedEvent2 =
            abstract range: IRange with get, set
            abstract rangeLength: float with get, set
            abstract text: string with get, set
            abstract eol: string with get, set
            abstract versionId: float with get, set
            abstract isUndoing: bool with get, set
            abstract isRedoing: bool with get, set

        and IModelContentChangedEvent =
            abstract changeType: string with get, set
            abstract versionId: float with get, set
            abstract isUndoing: bool with get, set
            abstract isRedoing: bool with get, set

        and IRawText =
            abstract length: float with get, set
            abstract lines: ResizeArray<string> with get, set
            abstract BOM: string with get, set
            abstract EOL: string with get, set
            abstract options: ITextModelResolvedOptions with get, set

        and IModelContentChangedFlushEvent =
            inherit IModelContentChangedEvent
            abstract detail: IRawText with get, set

        and IModelContentChangedLineChangedEvent =
            inherit IModelContentChangedEvent
            abstract lineNumber: float with get, set
            abstract detail: string with get, set

        and IModelContentChangedLinesDeletedEvent =
            inherit IModelContentChangedEvent
            abstract fromLineNumber: float with get, set
            abstract toLineNumber: float with get, set

        and IModelContentChangedLinesInsertedEvent =
            inherit IModelContentChangedEvent
            abstract fromLineNumber: float with get, set
            abstract toLineNumber: float with get, set
            abstract detail: string with get, set

        and IModelDecorationsChangedEventDecorationData =
            abstract id: string with get, set
            abstract ownerId: float with get, set
            abstract range: IRange with get, set
            abstract isForValidation: bool with get, set
            abstract options: IModelDecorationOptions with get, set

        and IModelDecorationsChangedEvent =
            abstract ids: ResizeArray<string> with get, set
            abstract addedOrChangedDecorations: ResizeArray<IModelDecorationsChangedEventDecorationData> with get, set
            abstract removedDecorations: ResizeArray<string> with get, set
            abstract oldOptions: obj with get, set
            abstract oldRanges: obj with get, set

        and IModelTokensChangedEvent =
            abstract fromLineNumber: float with get, set
            abstract toLineNumber: float with get, set

        and CursorChangeReason =
            | NotSet = 0
            | ContentFlush = 1
            | RecoverFromMarkers = 2
            | Explicit = 3
            | Paste = 4
            | Undo = 5
            | Redo = 6

        and ICursorPositionChangedEvent =
            abstract position: Position with get, set
            abstract viewPosition: Position with get, set
            abstract secondaryPositions: ResizeArray<Position> with get, set
            abstract secondaryViewPositions: ResizeArray<Position> with get, set
            abstract reason: CursorChangeReason with get, set
            abstract source: string with get, set
            abstract isInEditableRange: bool with get, set

        and ICursorSelectionChangedEvent =
            abstract selection: Selection with get, set
            abstract viewSelection: Selection with get, set
            abstract secondarySelections: ResizeArray<Selection> with get, set
            abstract secondaryViewSelections: ResizeArray<Selection> with get, set
            abstract source: string with get, set
            abstract reason: CursorChangeReason with get, set

        and IModelChangedEvent =
            abstract oldModelUrl: string with get, set
            abstract newModelUrl: string with get, set

        and [<Import("editor.OverviewRulerPosition","monaco")>] OverviewRulerPosition() =
            member __._overviewRulerPositionBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.top with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.right with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("editor.EditorLayoutInfo","monaco")>] EditorLayoutInfo() =
            member __._editorLayoutInfoBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.width with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.height with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.glyphMarginLeft with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.glyphMarginWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.glyphMarginHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.lineNumbersLeft with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.lineNumbersWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.lineNumbersHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.decorationsLeft with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.decorationsWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.decorationsHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.contentLeft with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.contentWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.contentHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.verticalScrollbarWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.horizontalScrollbarHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.overviewRuler with get(): OverviewRulerPosition = failwith "JS only" and set(v: OverviewRulerPosition): unit = failwith "JS only"

        and ICodeEditorWidgetCreationOptions =
            inherit IEditorOptions
            abstract model: IModel option with get, set

        and IEditorModel =
            interface end

        and IEditorViewState =
            interface end

        and IDimension =
            abstract width: float with get, set
            abstract height: float with get, set

        and IActionEnablement =
            abstract textFocus: bool option with get, set
            abstract widgetFocus: bool option with get, set
            abstract writeableEditor: bool option with get, set
            abstract tokensAtPosition: ResizeArray<string> option with get, set
            abstract wordAtPosition: bool option with get, set

        and ICursorState =
            abstract inSelectionMode: bool with get, set
            abstract selectionStart: IPosition with get, set
            abstract position: IPosition with get, set

        and IViewState =
            abstract scrollTop: float with get, set
            abstract scrollTopWithoutViewZones: float with get, set
            abstract scrollLeft: float with get, set

        and ICodeEditorViewState =
            inherit IEditorViewState
            abstract cursorState: ResizeArray<ICursorState> with get, set
            abstract viewState: IViewState with get, set
            abstract contributionsState: obj with get, set

        and MouseTargetType =
            | UNKNOWN = 0
            | TEXTAREA = 1
            | GUTTER_GLYPH_MARGIN = 2
            | GUTTER_LINE_NUMBERS = 3
            | GUTTER_LINE_DECORATIONS = 4
            | GUTTER_VIEW_ZONE = 5
            | CONTENT_TEXT = 6
            | CONTENT_EMPTY = 7
            | CONTENT_VIEW_ZONE = 8
            | CONTENT_WIDGET = 9
            | OVERVIEW_RULER = 10
            | SCROLLBAR = 11
            | OVERLAY_WIDGET = 12

        and IDiffEditorModel =
            inherit IEditorModel
            abstract original: IModel with get, set
            abstract modified: IModel with get, set

        and IDiffEditorViewState =
            inherit IEditorViewState
            abstract original: ICodeEditorViewState with get, set
            abstract modified: ICodeEditorViewState with get, set

        and IChange =
            abstract originalStartLineNumber: float with get, set
            abstract originalEndLineNumber: float with get, set
            abstract modifiedStartLineNumber: float with get, set
            abstract modifiedEndLineNumber: float with get, set

        and ICharChange =
            inherit IChange
            abstract originalStartColumn: float with get, set
            abstract originalEndColumn: float with get, set
            abstract modifiedStartColumn: float with get, set
            abstract modifiedEndColumn: float with get, set

        and ILineChange =
            inherit IChange
            abstract charChanges: ResizeArray<ICharChange> with get, set

        and [<Import("editor.BareFontInfo","monaco")>] BareFontInfo() =
            member __._bareFontInfoBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.fontFamily with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.fontSize with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.lineHeight with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and [<Import("editor.FontInfo","monaco")>] FontInfo() =
            inherit BareFontInfo()
            member __._editorStylingBrand with get(): unit = failwith "JS only" and set(v: unit): unit = failwith "JS only"
            member __.typicalHalfwidthCharacterWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.typicalFullwidthCharacterWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.spaceWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.maxDigitWidth with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

        and INewScrollPosition =
            abstract scrollLeft: float option with get, set
            abstract scrollTop: float option with get, set

        and IActionDescriptor =
            abstract id: string with get, set
            abstract label: string with get, set
            abstract keybindings: ResizeArray<float> option with get, set
            abstract keybindingContext: string option with get, set
            abstract enablement: IActionEnablement option with get, set
            abstract contextMenuGroupId: string option with get, set
            abstract run: Func<ICommonCodeEditor, Promise<unit>> with get, set

        and IEditor =
            abstract onDidChangeModelRawContent: listener: Func<IModelContentChangedEvent, unit> -> IDisposable
            abstract onDidChangeModelContent: listener: Func<IModelContentChangedEvent2, unit> -> IDisposable
            abstract onDidChangeModelMode: listener: Func<IModelModeChangedEvent, unit> -> IDisposable
            abstract onDidChangeModelOptions: listener: Func<IModelOptionsChangedEvent, unit> -> IDisposable
            abstract onDidChangeConfiguration: listener: Func<IConfigurationChangedEvent, unit> -> IDisposable
            abstract onDidChangeCursorPosition: listener: Func<ICursorPositionChangedEvent, unit> -> IDisposable
            abstract onDidChangeCursorSelection: listener: Func<ICursorSelectionChangedEvent, unit> -> IDisposable
            abstract onDidDispose: listener: Func<unit> -> IDisposable
            abstract dispose: unit -> unit
            abstract getId: unit -> string
            abstract getEditorType: unit -> string
            abstract updateOptions: newOptions: IEditorOptions -> unit
            abstract layout: ?dimension: IDimension -> unit
            abstract focus: unit -> unit
            abstract isFocused: unit -> bool
            abstract addAction: descriptor: IActionDescriptor -> unit
            abstract getActions: unit -> ResizeArray<IAction>
            abstract saveViewState: unit -> IEditorViewState
            abstract restoreViewState: state: IEditorViewState -> unit
            abstract getVisibleColumnFromPosition: position: IPosition -> float
            abstract getPosition: unit -> Position
            abstract setPosition: position: IPosition -> unit
            abstract revealLine: lineNumber: float -> unit
            abstract revealLineInCenter: lineNumber: float -> unit
            abstract revealLineInCenterIfOutsideViewport: lineNumber: float -> unit
            abstract revealPosition: position: IPosition -> unit
            abstract revealPositionInCenter: position: IPosition -> unit
            abstract revealPositionInCenterIfOutsideViewport: position: IPosition -> unit
            abstract getSelection: unit -> Selection
            abstract getSelections: unit -> ResizeArray<Selection>
            abstract setSelection: selection: IRange -> unit
            abstract setSelection: selection: Range -> unit
            abstract setSelection: selection: ISelection -> unit
            abstract setSelection: selection: Selection -> unit
            abstract setSelections: selections: ResizeArray<ISelection> -> unit
            abstract revealLines: startLineNumber: float * endLineNumber: float -> unit
            abstract revealLinesInCenter: lineNumber: float * endLineNumber: float -> unit
            abstract revealLinesInCenterIfOutsideViewport: lineNumber: float * endLineNumber: float -> unit
            abstract revealRange: range: IRange -> unit
            abstract revealRangeInCenter: range: IRange -> unit
            abstract revealRangeInCenterIfOutsideViewport: range: IRange -> unit
            abstract trigger: source: string * handlerId: string * payload: obj -> unit
            abstract getModel: unit -> IEditorModel
            abstract setModel: model: IEditorModel -> unit

        and IEditorContribution =
            abstract getId: unit -> string
            abstract dispose: unit -> unit
            abstract saveViewState: unit -> obj
            abstract restoreViewState: state: obj -> unit

        and ICommonCodeEditor =
            inherit IEditor
            abstract onDidChangeModel: listener: Func<IModelChangedEvent, unit> -> IDisposable
            abstract onDidChangeModelDecorations: listener: Func<IModelDecorationsChangedEvent, unit> -> IDisposable
            abstract onDidFocusEditorText: listener: Func<unit> -> IDisposable
            abstract onDidBlurEditorText: listener: Func<unit> -> IDisposable
            abstract onDidFocusEditor: listener: Func<unit> -> IDisposable
            abstract onDidBlurEditor: listener: Func<unit> -> IDisposable
            abstract hasWidgetFocus: unit -> bool
            abstract getContribution: id: string -> IEditorContribution
            abstract getModel: unit -> IModel
            abstract getConfiguration: unit -> InternalEditorOptions
            abstract getValue: ?options: obj -> string
            abstract setValue: newValue: string -> unit
            abstract getScrollWidth: unit -> float
            abstract getScrollLeft: unit -> float
            abstract getScrollHeight: unit -> float
            abstract getScrollTop: unit -> float
            abstract setScrollLeft: newScrollLeft: float -> unit
            abstract setScrollTop: newScrollTop: float -> unit
            abstract setScrollPosition: position: INewScrollPosition -> unit
            abstract getAction: id: string -> IAction
            abstract executeCommand: source: string * command: ICommand -> unit
            abstract executeEdits: source: string * edits: ResizeArray<IIdentifiedSingleEditOperation> -> bool
            abstract executeCommands: source: string * commands: ResizeArray<ICommand> -> unit
            abstract getLineDecorations: lineNumber: float -> ResizeArray<IModelDecoration>
            abstract deltaDecorations: oldDecorations: ResizeArray<string> * newDecorations: ResizeArray<IModelDeltaDecoration> -> ResizeArray<string>
            abstract getLayoutInfo: unit -> EditorLayoutInfo

        and ICommonDiffEditor =
            inherit IEditor
            abstract onDidUpdateDiff: listener: Func<unit> -> IDisposable
            abstract getModel: unit -> IDiffEditorModel
            abstract getOriginalEditor: unit -> ICommonCodeEditor
            abstract getModifiedEditor: unit -> ICommonCodeEditor
            abstract getLineChanges: unit -> ResizeArray<ILineChange>
            abstract getValue: ?options: obj -> string

        and EditorTypeType =
            abstract ICodeEditor: string with get, set
            abstract IDiffEditor: string with get, set

        and HandlerType =
            abstract ExecuteCommand: string with get, set
            abstract ExecuteCommands: string with get, set
            abstract CursorLeft: string with get, set
            abstract CursorLeftSelect: string with get, set
            abstract CursorWordLeft: string with get, set
            abstract CursorWordStartLeft: string with get, set
            abstract CursorWordEndLeft: string with get, set
            abstract CursorWordLeftSelect: string with get, set
            abstract CursorWordStartLeftSelect: string with get, set
            abstract CursorWordEndLeftSelect: string with get, set
            abstract CursorRight: string with get, set
            abstract CursorRightSelect: string with get, set
            abstract CursorWordRight: string with get, set
            abstract CursorWordStartRight: string with get, set
            abstract CursorWordEndRight: string with get, set
            abstract CursorWordRightSelect: string with get, set
            abstract CursorWordStartRightSelect: string with get, set
            abstract CursorWordEndRightSelect: string with get, set
            abstract CursorUp: string with get, set
            abstract CursorUpSelect: string with get, set
            abstract CursorDown: string with get, set
            abstract CursorDownSelect: string with get, set
            abstract CursorPageUp: string with get, set
            abstract CursorPageUpSelect: string with get, set
            abstract CursorPageDown: string with get, set
            abstract CursorPageDownSelect: string with get, set
            abstract CursorHome: string with get, set
            abstract CursorHomeSelect: string with get, set
            abstract CursorEnd: string with get, set
            abstract CursorEndSelect: string with get, set
            abstract ExpandLineSelection: string with get, set
            abstract CursorTop: string with get, set
            abstract CursorTopSelect: string with get, set
            abstract CursorBottom: string with get, set
            abstract CursorBottomSelect: string with get, set
            abstract CursorColumnSelectLeft: string with get, set
            abstract CursorColumnSelectRight: string with get, set
            abstract CursorColumnSelectUp: string with get, set
            abstract CursorColumnSelectPageUp: string with get, set
            abstract CursorColumnSelectDown: string with get, set
            abstract CursorColumnSelectPageDown: string with get, set
            abstract AddCursorDown: string with get, set
            abstract AddCursorUp: string with get, set
            abstract CursorUndo: string with get, set
            abstract MoveTo: string with get, set
            abstract MoveToSelect: string with get, set
            abstract ColumnSelect: string with get, set
            abstract CreateCursor: string with get, set
            abstract LastCursorMoveToSelect: string with get, set
            abstract JumpToBracket: string with get, set
            abstract Type: string with get, set
            abstract ReplacePreviousChar: string with get, set
            abstract Paste: string with get, set
            abstract Tab: string with get, set
            abstract Indent: string with get, set
            abstract Outdent: string with get, set
            abstract DeleteLeft: string with get, set
            abstract DeleteRight: string with get, set
            abstract DeleteWordLeft: string with get, set
            abstract DeleteWordStartLeft: string with get, set
            abstract DeleteWordEndLeft: string with get, set
            abstract DeleteWordRight: string with get, set
            abstract DeleteWordStartRight: string with get, set
            abstract DeleteWordEndRight: string with get, set
            abstract DeleteAllLeft: string with get, set
            abstract DeleteAllRight: string with get, set
            abstract RemoveSecondaryCursors: string with get, set
            abstract CancelSelection: string with get, set
            abstract Cut: string with get, set
            abstract Undo: string with get, set
            abstract Redo: string with get, set
            abstract WordSelect: string with get, set
            abstract WordSelectDrag: string with get, set
            abstract LastCursorWordSelect: string with get, set
            abstract LineSelect: string with get, set
            abstract LineSelectDrag: string with get, set
            abstract LastCursorLineSelect: string with get, set
            abstract LastCursorLineSelectDrag: string with get, set
            abstract LineInsertBefore: string with get, set
            abstract LineInsertAfter: string with get, set
            abstract LineBreakInsert: string with get, set
            abstract SelectAll: string with get, set
            abstract ScrollLineUp: string with get, set
            abstract ScrollLineDown: string with get, set
            abstract ScrollPageUp: string with get, set
            abstract ScrollPageDown: string with get, set

        and TextEditorCursorStyle =
            | Line = 1
            | Block = 2
            | Underline = 3

        and IViewZone =
            abstract afterLineNumber: float with get, set
            abstract afterColumn: float option with get, set
            abstract suppressMouseDown: bool option with get, set
            abstract heightInLines: float option with get, set
            abstract heightInPx: float option with get, set
            abstract domNode: HTMLElement with get, set
            abstract onDomNodeTop: Func<float, unit> option with get, set
            abstract onComputedHeight: Func<float, unit> option with get, set

        and IViewZoneChangeAccessor =
            abstract addZone: zone: IViewZone -> float
            abstract removeZone: id: float -> unit
            abstract layoutZone: id: float -> unit

        and ContentWidgetPositionPreference =
            | EXACT = 0
            | ABOVE = 1
            | BELOW = 2

        and IContentWidgetPosition =
            abstract position: IPosition with get, set
            abstract preference: ResizeArray<ContentWidgetPositionPreference> with get, set

        and IContentWidget =
            abstract allowEditorOverflow: bool option with get, set
            abstract getId: unit -> string
            abstract getDomNode: unit -> HTMLElement
            abstract getPosition: unit -> IContentWidgetPosition

        and OverlayWidgetPositionPreference =
            | TOP_RIGHT_CORNER = 0
            | BOTTOM_RIGHT_CORNER = 1
            | TOP_CENTER = 2

        and IOverlayWidgetPosition =
            abstract preference: OverlayWidgetPositionPreference with get, set

        and IOverlayWidget =
            abstract getId: unit -> string
            abstract getDomNode: unit -> HTMLElement
            abstract getPosition: unit -> IOverlayWidgetPosition

        and IMouseTarget =
            abstract element: Element with get, set
            abstract ``type``: MouseTargetType with get, set
            abstract position: Position with get, set
            abstract mouseColumn: float with get, set
            abstract range: Range with get, set
            abstract detail: obj with get, set

        and IEditorMouseEvent =
            abstract ``event``: IMouseEvent with get, set
            abstract target: IMouseTarget with get, set

        and ICodeEditor =
            inherit ICommonCodeEditor
            abstract onMouseUp: listener: Func<IEditorMouseEvent, unit> -> IDisposable
            abstract onMouseDown: listener: Func<IEditorMouseEvent, unit> -> IDisposable
            abstract onContextMenu: listener: Func<IEditorMouseEvent, unit> -> IDisposable
            abstract onMouseMove: listener: Func<IEditorMouseEvent, unit> -> IDisposable
            abstract onMouseLeave: listener: Func<IEditorMouseEvent, unit> -> IDisposable
            abstract onKeyUp: listener: Func<IKeyboardEvent, unit> -> IDisposable
            abstract onKeyDown: listener: Func<IKeyboardEvent, unit> -> IDisposable
            abstract onDidLayoutChange: listener: Func<EditorLayoutInfo, unit> -> IDisposable
            abstract onDidScrollChange: listener: Func<IScrollEvent, unit> -> IDisposable
            abstract getDomNode: unit -> HTMLElement
            abstract addContentWidget: widget: IContentWidget -> unit
            abstract layoutContentWidget: widget: IContentWidget -> unit
            abstract removeContentWidget: widget: IContentWidget -> unit
            abstract addOverlayWidget: widget: IOverlayWidget -> unit
            abstract layoutOverlayWidget: widget: IOverlayWidget -> unit
            abstract removeOverlayWidget: widget: IOverlayWidget -> unit
            abstract changeViewZones: callback: Func<IViewZoneChangeAccessor, unit> -> unit
            abstract getCenteredRangeInViewport: unit -> Range
            abstract getOffsetForColumn: lineNumber: float * column: float -> float
            abstract render: unit -> unit
            abstract getTopForLineNumber: lineNumber: float -> float
            abstract getTopForPosition: lineNumber: float * column: float -> float
            abstract getScrolledVisiblePosition: position: IPosition -> obj
            abstract applyFontInfo: target: HTMLElement -> unit

        and IDiffEditor =
            inherit ICommonDiffEditor
            abstract getDomNode: unit -> HTMLElement

        type [<Import("editor","monaco")>] Globals =
            static member KEYBINDING_CONTEXT_EDITOR_TEXT_FOCUS with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member KEYBINDING_CONTEXT_EDITOR_FOCUS with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member KEYBINDING_CONTEXT_EDITOR_HAS_MULTIPLE_SELECTIONS with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member KEYBINDING_CONTEXT_EDITOR_HAS_NON_EMPTY_SELECTION with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member KEYBINDING_CONTEXT_EDITOR_LANGUAGE_ID with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member EditorType with get(): EditorTypeType = failwith "JS only" and set(v: EditorTypeType): unit = failwith "JS only"
            static member Handler with get(): HandlerType = failwith "JS only" and set(v: HandlerType): unit = failwith "JS only"
            static member create(domElement: HTMLElement, options: IEditorConstructionOptions, services: IEditorOverrideServices): ICodeEditor = failwith "JS only"
            static member createDiffEditor(domElement: HTMLElement, options: IDiffEditorConstructionOptions, services: IEditorOverrideServices): IDiffEditor = failwith "JS only"
            static member createModel(value: string, ?language: string, ?uri: Uri): IModel = failwith "JS only"
            static member setModelLanguage(model: IModel, language: string): unit = failwith "JS only"
            static member setModelMarkers(model: IModel, owner: string, markers: ResizeArray<IMarkerData>): unit = failwith "JS only"
            static member getModel(uri: Uri): IModel = failwith "JS only"
            static member getModels(): ResizeArray<IModel> = failwith "JS only"
            static member onDidCreateModel(listener: Func<IModel, unit>): IDisposable = failwith "JS only"
            static member onWillDisposeModel(listener: Func<IModel, unit>): IDisposable = failwith "JS only"
            static member onDidChangeModelLanguage(listener: Func<obj, unit>): IDisposable = failwith "JS only"
            static member createWebWorker(opts: IWebWorkerOptions): MonacoWebWorker<'T> = failwith "JS only"
            static member colorizeElement(domNode: HTMLElement, options: IColorizerElementOptions): Promise<unit> = failwith "JS only"
            static member colorize(text: string, modeId: string, options: IColorizerOptions): Promise<string> = failwith "JS only"
            static member colorizeModelLine(model: IModel, lineNumber: float, ?tabSize: float): string = failwith "JS only"



    module languages =
        type CompletionItemKind =
            | Text = 0
            | Method = 1
            | Function = 2
            | Constructor = 3
            | Field = 4
            | Variable = 5
            | Class = 6
            | Interface = 7
            | Module = 8
            | Property = 9
            | Unit = 10
            | Value = 11
            | Enum = 12
            | Keyword = 13
            | Snippet = 14
            | Color = 15
            | File = 16
            | Reference = 17

        and CompletionItem =
            abstract label: string with get, set
            abstract kind: CompletionItemKind with get, set
            abstract detail: string option with get, set
            abstract documentation: string option with get, set
            abstract sortText: string option with get, set
            abstract filterText: string option with get, set
            abstract insertText: string option with get, set
            abstract textEdit: editor.ISingleEditOperation option with get, set

        and CompletionList =
            abstract isIncomplete: bool option with get, set
            abstract items: ResizeArray<CompletionItem> with get, set

        and CompletionItemProvider =
            abstract triggerCharacters: ResizeArray<string> option with get//, set
            abstract provideCompletionItems: model: editor.IReadOnlyModel * position: Position * token: CancellationToken -> U4<ResizeArray<CompletionItem>, Promise<ResizeArray<CompletionItem>>, CompletionList, Promise<CompletionList>>
            abstract resolveCompletionItem: item: CompletionItem * token: CancellationToken -> U2<CompletionItem, Promise<CompletionItem>>

        and CommentRule =
            abstract lineComment: string option with get, set
            abstract blockComment: CharacterPair option with get, set

        and IRichLanguageConfiguration =
            abstract comments: CommentRule option with get, set
            abstract brackets: ResizeArray<CharacterPair> option with get, set
            abstract wordPattern: Regex option with get, set
            abstract indentationRules: IIndentationRules option with get, set
            abstract onEnterRules: ResizeArray<IOnEnterRegExpRules> option with get, set
            abstract autoClosingPairs: ResizeArray<IAutoClosingPairConditional> option with get, set
            abstract surroundingPairs: ResizeArray<IAutoClosingPair> option with get, set
            abstract ___electricCharacterSupport: IBracketElectricCharacterContribution option with get, set

        and IIndentationRules =
            abstract decreaseIndentPattern: Regex with get, set
            abstract increaseIndentPattern: Regex with get, set
            abstract indentNextLinePattern: Regex option with get, set
            abstract unIndentedLinePattern: Regex option with get, set

        and IOnEnterRegExpRules =
            abstract beforeText: Regex with get, set
            abstract afterText: Regex option with get, set
            abstract action: IEnterAction with get, set

        and IBracketElectricCharacterContribution =
            abstract docComment: IDocComment option with get, set
            abstract embeddedElectricCharacters: ResizeArray<string> option with get, set

        and IDocComment =
            abstract scope: string with get, set
            abstract ``open``: string with get, set
            abstract lineStart: string with get, set
            abstract close: string option with get, set

        and IMode =
            abstract getId: unit -> string

        and IToken =
            abstract startIndex: float with get, set
            abstract scopes: U2<string, ResizeArray<string>> with get, set

        and ILineTokens =
            abstract tokens: ResizeArray<IToken> with get, set
            abstract endState: IState with get, set
            abstract retokenize: Promise<unit> option with get, set

        and IState =
            abstract clone: unit -> IState
            abstract equals: other: IState -> bool

        and TokensProvider =
            abstract getInitialState: unit -> IState
            abstract tokenize: line: string * state: IState -> ILineTokens

        and Hover =
            abstract htmlContent: ResizeArray<IHTMLContentElement> with get, set
            abstract range: IRange with get, set

        and HoverProvider =
            abstract provideHover: model: editor.IReadOnlyModel * position: Position * token: CancellationToken -> U2<Hover, Promise<Hover>>

        and IQuickFix =
            abstract command: ICommand with get, set
            abstract score: float with get, set

        and CodeActionProvider =
            abstract provideCodeActions: model: editor.IReadOnlyModel * range: Range * token: CancellationToken -> U2<ResizeArray<IQuickFix>, Promise<ResizeArray<IQuickFix>>>

        and ParameterInformation =
            abstract label: string with get, set
            abstract documentation: string with get, set

        and SignatureInformation =
            abstract label: string with get, set
            abstract documentation: string with get, set
            abstract parameters: ResizeArray<ParameterInformation> with get, set

        and SignatureHelp =
            abstract signatures: ResizeArray<SignatureInformation> with get, set
            abstract activeSignature: float with get, set
            abstract activeParameter: float with get, set

        and SignatureHelpProvider =
            abstract signatureHelpTriggerCharacters: ResizeArray<string> with get//, set
            abstract provideSignatureHelp: model: editor.IReadOnlyModel * position: Position * token: CancellationToken -> U2<SignatureHelp, Promise<SignatureHelp>>

        and DocumentHighlightKind =
            | Text = 0
            | Read = 1
            | Write = 2

        and DocumentHighlight =
            abstract range: IRange with get, set
            abstract kind: DocumentHighlightKind with get, set

        and DocumentHighlightProvider =
            abstract provideDocumentHighlights: model: editor.IReadOnlyModel * position: Position * token: CancellationToken -> U2<ResizeArray<DocumentHighlight>, Promise<ResizeArray<DocumentHighlight>>>

        and ReferenceContext =
            abstract includeDeclaration: bool with get, set

        and ReferenceProvider =
            abstract provideReferences: model: editor.IReadOnlyModel * position: Position * context: ReferenceContext * token: CancellationToken -> U2<ResizeArray<Location>, Promise<ResizeArray<Location>>>

        and Location =
            abstract uri: Uri with get, set
            abstract range: IRange with get, set

        and Definition =
            U2<Location, ResizeArray<Location>>

        and DefinitionProvider =
            abstract provideDefinition: model: editor.IReadOnlyModel * position: Position * token: CancellationToken -> U2<Definition, Promise<Definition>>

        and SymbolKind =
            | File = 0
            | Module = 1
            | Namespace = 2
            | Package = 3
            | Class = 4
            | Method = 5
            | Property = 6
            | Field = 7
            | Constructor = 8
            | Enum = 9
            | Interface = 10
            | Function = 11
            | Variable = 12
            | Constant = 13
            | String = 14
            | Number = 15
            | Boolean = 16
            | Array = 17
            | Object = 18
            | Key = 19
            | Null = 20

        and SymbolInformation =
            abstract name: string with get, set
            abstract containerName: string option with get, set
            abstract kind: SymbolKind with get, set
            abstract location: Location with get, set

        and DocumentSymbolProvider =
            abstract provideDocumentSymbols: model: editor.IReadOnlyModel * token: CancellationToken -> U2<ResizeArray<SymbolInformation>, Promise<ResizeArray<SymbolInformation>>>

        and IFormattingOptions =
            abstract tabSize: float with get, set
            abstract insertSpaces: bool with get, set

        and DocumentFormattingEditProvider =
            abstract provideDocumentFormattingEdits: model: editor.IReadOnlyModel * options: IFormattingOptions * token: CancellationToken -> U2<ResizeArray<editor.ISingleEditOperation>, Promise<ResizeArray<editor.ISingleEditOperation>>>

        and DocumentRangeFormattingEditProvider =
            abstract provideDocumentRangeFormattingEdits: model: editor.IReadOnlyModel * range: Range * options: IFormattingOptions * token: CancellationToken -> U2<ResizeArray<editor.ISingleEditOperation>, Promise<ResizeArray<editor.ISingleEditOperation>>>

        and OnTypeFormattingEditProvider =
            abstract autoFormatTriggerCharacters: ResizeArray<string> with get, set
            abstract provideOnTypeFormattingEdits: model: editor.IReadOnlyModel * position: Position * ch: string * options: IFormattingOptions * token: CancellationToken -> U2<ResizeArray<editor.ISingleEditOperation>, Promise<ResizeArray<editor.ISingleEditOperation>>>

        and ILink =
            abstract range: IRange with get, set
            abstract url: string with get, set

        and LinkProvider =
            abstract provideLinks: model: editor.IReadOnlyModel * token: CancellationToken -> U2<ResizeArray<ILink>, Promise<ResizeArray<ILink>>>

        and IResourceEdit =
            abstract resource: Uri with get, set
            abstract range: IRange with get, set
            abstract newText: string with get, set

        and WorkspaceEdit =
            abstract edits: ResizeArray<IResourceEdit> with get, set
            abstract rejectReason: string option with get, set

        and RenameProvider =
            abstract provideRenameEdits: model: editor.IReadOnlyModel * position: Position * newName: string * token: CancellationToken -> U2<WorkspaceEdit, Promise<WorkspaceEdit>>

        and ICommand =
            abstract id: string with get, set
            abstract title: string with get, set
            abstract arguments: ResizeArray<obj> option with get, set

        and ICodeLensSymbol =
            abstract range: IRange with get, set
            abstract id: string option with get, set
            abstract command: ICommand option with get, set

        and CodeLensProvider =
            abstract provideCodeLenses: model: editor.IReadOnlyModel * token: CancellationToken -> U2<ResizeArray<ICodeLensSymbol>, Promise<ResizeArray<ICodeLensSymbol>>>
            abstract resolveCodeLens: model: editor.IReadOnlyModel * codeLens: ICodeLensSymbol * token: CancellationToken -> U2<ICodeLensSymbol, Promise<ICodeLensSymbol>>

        and CharacterPair =
            string * string

        and IAutoClosingPairConditional =
            inherit IAutoClosingPair
            abstract notIn: ResizeArray<string> option with get, set

        and IndentAction =
            | None = 0
            | Indent = 1
            | IndentOutdent = 2
            | Outdent = 3

        and IEnterAction =
            abstract indentAction: IndentAction with get, set
            abstract appendText: string option with get, set
            abstract removeText: float option with get, set

        and IAutoClosingPair =
            abstract ``open``: string with get, set
            abstract close: string with get, set

        and ILanguageExtensionPoint =
            abstract id: string with get, set
            abstract extensions: ResizeArray<string> option with get, set
            abstract filenames: ResizeArray<string> option with get, set
            abstract filenamePatterns: ResizeArray<string> option with get, set
            abstract firstLine: string option with get, set
            abstract aliases: ResizeArray<string> option with get, set
            abstract mimetypes: ResizeArray<string> option with get, set
            abstract configuration: string option with get, set

        and IMonarchLanguage =
            abstract tokenizer: obj with get, set
            abstract ignoreCase: bool option with get, set
            abstract defaultToken: string option with get, set
            abstract brackets: ResizeArray<IMonarchLanguageBracket> option with get, set
            abstract start: string option with get, set
            abstract tokenPostfix: string with get, set

        and IMonarchLanguageRule =
            abstract regex: U2<string, Regex> option with get, set
            abstract action: IMonarchLanguageAction option with get, set
            abstract ``include``: string option with get, set

        and IMonarchLanguageAction =
            abstract group: ResizeArray<IMonarchLanguageAction> option with get, set
            abstract cases: obj option with get, set
            abstract token: string option with get, set
            abstract next: string option with get, set
            abstract switchTo: string option with get, set
            abstract goBack: float option with get, set
            abstract bracket: string option with get, set
            abstract nextEmbedded: string option with get, set
            abstract log: string option with get, set

        and IMonarchLanguageBracket =
            abstract ``open``: string with get, set
            abstract close: string with get, set
            abstract token: string with get, set

        type [<Import("languages","monaco")>] Globals =
            static member register(language: ILanguageExtensionPoint): unit = failwith "JS only"
            static member getLanguages(): ResizeArray<ILanguageExtensionPoint> = failwith "JS only"
            static member onLanguage(languageId: string, callback: Func<unit>): IDisposable = failwith "JS only"
            static member setLanguageConfiguration(languageId: string, configuration: IRichLanguageConfiguration): IDisposable = failwith "JS only"
            static member setTokensProvider(languageId: string, support: TokensProvider): IDisposable = failwith "JS only"
            static member setMonarchTokensProvider(languageId: string, languageDef: IMonarchLanguage): IDisposable = failwith "JS only"
            static member registerReferenceProvider(languageId: string, support: ReferenceProvider): IDisposable = failwith "JS only"
            static member registerRenameProvider(languageId: string, support: RenameProvider): IDisposable = failwith "JS only"
            static member registerCompletionItemProvider(languageId: string, provider: CompletionItemProvider): IDisposable = failwith "JS only"
            static member registerSignatureHelpProvider(languageId: string, support: SignatureHelpProvider): IDisposable = failwith "JS only"
            static member registerHoverProvider(languageId: string, support: HoverProvider): IDisposable = failwith "JS only"
            static member registerDocumentSymbolProvider(languageId: string, support: DocumentSymbolProvider): IDisposable = failwith "JS only"
            static member registerDocumentHighlightProvider(languageId: string, support: DocumentHighlightProvider): IDisposable = failwith "JS only"
            static member registerDefinitionProvider(languageId: string, support: DefinitionProvider): IDisposable = failwith "JS only"
            static member registerCodeLensProvider(languageId: string, support: CodeLensProvider): IDisposable = failwith "JS only"
            static member registerCodeActionProvider(languageId: string, support: CodeActionProvider): IDisposable = failwith "JS only"
            static member registerDocumentFormattingEditProvider(languageId: string, support: DocumentFormattingEditProvider): IDisposable = failwith "JS only"
            static member registerDocumentRangeFormattingEditProvider(languageId: string, support: DocumentRangeFormattingEditProvider): IDisposable = failwith "JS only"
            static member registerOnTypeFormattingEditProvider(languageId: string, support: OnTypeFormattingEditProvider): IDisposable = failwith "JS only"
            static member registerLinkProvider(languageId: string, support: LinkProvider): IDisposable = failwith "JS only"



    module worker =
        type IMirrorModel =
            abstract uri: Uri with get, set
            abstract version: float with get, set
            abstract getText: unit -> string

        type [<Import("worker","monaco")>] Globals =
            static member mirrorModels with get(): ResizeArray<IMirrorModel> = failwith "JS only" and set(v: ResizeArray<IMirrorModel>): unit = failwith "JS only"
