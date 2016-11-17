// XE4 introduces the ability to terminate $IF blocks with $ENDIF,
// and the next-generation (i.e. mobile) compilers deprecate $IFEND
// altogether. This directive suppresses deprecation warnings on
// next-generation compilers.
{$IF CompilerVersion >= 25}
  {$LEGACYIFEND ON}
{$IFEND}

// 2009 introduces TStringBuilder for efficient, future-proof
// concatenation. (Strings will eventually be made immutable on
// next-generation compilers.)
{$IF CompilerVersion >= 21}
  {$DEFINE STRINGBUILDER}
{$IFEND}

// While generics were introduced in 2009, using them in that version
// breaks the build in mysterious ways, so we only enable generics for
// 2010 and above.
{$IF CompilerVersion >= 21}
  {$DEFINE GENERICS}
{$IFEND}

// XE2 and above have built-in ASSEMBLER and CPUX* conditionals. On XE
// and below, assembly is implicitly supported on the Win32 platform.
{$IF CompilerVersion < 23}
  {$IFDEF WIN32}
    {$DEFINE ASSEMBLER}
    {$DEFINE CPUX86}
  {$ENDIF}
{$IFEND}

// XE2 introduces the TEncoding.ANSI character encoding.
{$IF CompilerVersion >= 23}
  {$DEFINE ANSI}
{$IFEND}

// XE3 introduces TStringHelper and support for long strings in Low().
{$IF CompilerVersion >= 24}
  {$DEFINE STRINGHELPER}
{$IFEND}

