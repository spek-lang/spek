# frozen_string_literal: true
#
# Rouge lexer for the Spek language (github.com/spek-lang/spek, an
# actor-model language for .NET). Jekyll loads _plugins/ at startup, and
# defining the class under Rouge::Lexers lets Rouge resolve ```spek fenced
# blocks by tag.
#
# GitHub Pages' default build runs Jekyll in --safe mode and ignores
# _plugins/, so this lexer only takes effect in a build that loads plugins:
# the Actions Pages workflow (.github/workflows/docs.yml) or a local
# `bundle exec jekyll build/serve`. See docs/README.md.

require 'rouge'

module Rouge
  module Lexers
    class Spek < RegexLexer
      title 'Spek'
      desc 'The Spek actor-model language for .NET'
      tag 'spek'
      filenames '*.spek'
      mimetypes 'text/x-spek'

      # Hard keywords from Spek.Compiler/Grammar/SpekLexer.g4.
      def self.keywords
        @keywords ||= Set.new %w(
          abstract actor any become behavior channel class else emits enum
          for foreach if init internal message namespace new on override passivate
          persist private program protected public return spawn supervise
          using interop reader writer try catch finally throw when where switch
          not and or is as event module in ref out shared term transient
          deprecated retired use var void while do break continue after strategy
        )
      end

      # Contextual / pseudo values that read like literals or "this".
      def self.keywords_pseudo
        @keywords_pseudo ||= Set.new %w(self sender true false null)
      end

      # Lifecycle and supervision hard keywords; PascalCase /
      # camelCase names the grammar reserves.
      def self.builtins
        @builtins ||= Set.new %w(
          Restore PreStart PostStop Failure Restart Stop Escalate
          OneForOne AllForOne maxRetries withinTime
        )
      end

      # Common primitive / BCL value types. Spek types are plain identifiers,
      # so this is a curated highlight set, not an exhaustive grammar rule.
      def self.types
        @types ||= Set.new %w(
          bool byte sbyte char decimal double float int uint long ulong
          short ushort string object void
          ActorRef Snapshot Task ValueTask
        )
      end

      state :root do
        rule %r/\s+/, Text::Whitespace
        rule %r(//.*?$),       Comment::Single
        rule %r(/\*.*?\*/)m,   Comment::Multiline

        # Strings, most specific delimiter first.
        rule %r/"""/,            Str::Heredoc, :raw_string
        rule %r/(?:\$@|@\$)"/,   Str::Double,  :verbatim_string   # verbatim-interpolated
        rule %r/@"/,             Str::Double,  :verbatim_string
        rule %r/\$"/,            Str::Double,  :interp_string
        rule %r/"/,              Str::Double,  :string
        rule %r/'(?:\\.|[^'\\])'/, Str::Char

        # Numeric literals, C#-aligned: separators, hex, binary, suffixes.
        rule %r/0[xX][0-9a-fA-F_]+[uUlL]*/, Num::Hex
        rule %r/0[bB][01_]+[uUlL]*/,        Num::Bin
        rule %r/[0-9][0-9_]*\.[0-9_]+(?:[eE][+-]?[0-9_]+)?[fFdDmM]?/, Num::Float
        rule %r/[0-9][0-9_]*[eE][+-]?[0-9_]+[fFdDmM]?/, Num::Float
        rule %r/[0-9][0-9_]*[fFdDmM]/,       Num::Float
        rule %r/[0-9][0-9_]*[uUlL]*/,        Num::Integer

        # Identifiers and keywords.
        rule %r/[a-zA-Z_]\w*/ do |m|
          name = m[0]
          if self.class.keywords.include?(name)            then token Keyword
          elsif self.class.keywords_pseudo.include?(name)  then token Keyword::Pseudo
          elsif self.class.builtins.include?(name)         then token Name::Builtin
          elsif self.class.types.include?(name)            then token Keyword::Type
          else token Name
          end
        end

        # Operators and punctuation.
        rule %r/=>|[+\-*\/%]=|==|!=|<=|>=|&&|\|\||[+\-*\/%=!<>?]/, Operator
        rule %r/[.,;:(){}\[\]]/, Punctuation

        # Graceful fallback: anything unrecognised (a stray char, unsupported
        # syntax, a Unicode glyph) renders as plain text rather
        # than an Error token. The doc-snippet test harness is the validator;
        # this lexer is only the highlighter, so it should never choke.
        rule %r/./m, Text
      end

      state :string do
        rule %r/[^"\\]+/, Str::Double
        rule %r/\\./,     Str::Escape
        rule %r/"/,       Str::Double, :pop!
      end

      # Verbatim: `""` is an escaped quote; backslashes are literal.
      state :verbatim_string do
        rule %r/""/,      Str::Escape
        rule %r/[^"]+/,   Str::Double
        rule %r/"/,       Str::Double, :pop!
      end

      # Interpolated: highlight `{ … }` holes; `\` escapes; whole rest is string.
      state :interp_string do
        rule %r/\{\{/,    Str::Double
        rule %r/\}\}/,    Str::Double
        rule %r/\{/,      Str::Interpol, :interp_hole
        rule %r/\\./,     Str::Escape
        rule %r/[^"\\{]+/, Str::Double
        rule %r/"/,       Str::Double, :pop!
      end

      state :interp_hole do
        rule %r/\}/,      Str::Interpol, :pop!
        mixin :root
      end

      state :raw_string do
        rule %r/"""/,     Str::Heredoc, :pop!
        rule %r/[^"]+/,   Str::Heredoc
        rule %r/"/,       Str::Heredoc
      end
    end
  end
end
