/* This adds a parser interface on top of MarkdownSharp's Markdown.cs. 
 * In addition to the original purpose of translating Markdown into html (textually), this also adds
 * a parsing interface over the translation logic, so we can use it for purposes other than generating html
 * (such as the markdown classifier).
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using MarkdownSharp;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Text.Tagging;

namespace MarkdownMode
{
    static class RegexExtensions
    {
        public static string ReplaceWithDummy(this Regex regex, string text, Action<Match> evaluator)
        {
            return regex.Replace(text, match =>
                {
                    evaluator(match);
                    return new string(MarkdownParser._dummyChar, match.Length);
                });
        }
    }

    static class MarkdownParser
    {
        internal const char _dummyChar = '~';

        // The regexes below are generally copies of regexes in Markdown.cs, except that they have been modified to allow for
        // endlines other than \n.

        const string ParagraphEndRegexPart = @"(?:(?:(?:\r\n){1,}|\r{1,}|\n{1,})|\Z)";
        const string ListItemEndlinePart = @"(?:(?:\r\n){1,2}|\r{1,2}|\n{1,2})";
        const string EndlinePart = @"(?:(?:\r\n)|\r|\n)";

        static Regex UlListItemRegex = new Regex(
            @"(\n)?                      # leading line = $1
              (^[ \t]*)                  # leading whitespace = $2
              (" + Markdown.MarkerUL + @") [ \t]+ # list marker = $3
              ((?s:.+?)                  # list item text = $4
               (" + ListItemEndlinePart + @"))
              (?= " + EndlinePart + @"* (\z | \2 (" + Markdown.MarkerUL + @") [ \t]+))", RegexOptions.Compiled | RegexOptions.IgnorePatternWhitespace | RegexOptions.Multiline);

        static Regex OlListItemRegex = new Regex(@"
              (\n)?                      # leading line = $1
              (^[ \t]*)                  # leading whitespace = $2
              (" + Markdown.MarkerOL + @") [ \t]+ # list marker = $3
              ((?s:.+?)                  # list item text = $4
               (" + ListItemEndlinePart + @"))
              (?= " + EndlinePart + @"* (\z | \2 (" + Markdown.MarkerOL + @") [ \t]+))", RegexOptions.Compiled | RegexOptions.IgnorePatternWhitespace | RegexOptions.Multiline);

        const string ParagraphStartRegexPart = @"
            (?:
               (?<=                       # Starts with two consecutive blank lines (ignore whitespace between them)
                 " + EndlinePart + @" 
                 [ \t]*
                 " + EndlinePart + @"
               )
               |                          # ... or it starts at the beginning of the string, followed by an optional newline
               \A" + EndlinePart + @"?)";


        static Regex ParserListTopLevelRegex = new Regex(ParagraphStartRegexPart + Markdown.WholeListRegex,
            RegexOptions.Multiline | RegexOptions.IgnorePatternWhitespace | RegexOptions.Compiled);

        internal static Regex ParserCodeBlockRegex = new Regex(string.Format(ParagraphStartRegexPart + @"
                    (                        # $1 = the code block -- one or more lines, starting with a space/tab
                    (?:
                        (?:[ ]{{{0}}} | \t)  # Lines must start with a tab or a tab-width of spaces
                        .*\n+
                    )+
                    )
                    ((?=^[ ]{{0,{0}}}\S)|{1}) # Lookahead for non-space at line-start, or end of doc",
                    Markdown.TabWidth, ParagraphEndRegexPart), RegexOptions.Multiline | RegexOptions.IgnorePatternWhitespace | RegexOptions.Compiled);

        #region Markdown public parser interface

        /// <summary>
        /// Parses the given Markdown-formatted paragraph into tokens.
        /// </summary>
        /// <param name="text">The paragraph of text to parse</param>
        /// <param name="offset">An optional offset that all the generated tokens will use.</param>
        /// <returns>An enumeration of tokens parsed from the text.</returns>
        public static IEnumerable<Token> ParseMarkdownParagraph(string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            if (text.Trim().Length == 0)
                return tokens;

            // First, write over the html tags with dummy characters so we ignore them
            text = DestroyHtmlTags(text);

            // Parse the paragraph into parts.  Note that the text will be modified in each step, but
            // characters will not be added or removed (so that the token locations as offsets into the text
            // will be accurate to the original text).
            tokens.AddRange(ParseHeaders(ref text, offset));
            tokens.AddRange(ParseHorizontalRules(ref text, offset));
            tokens.AddRange(ParseLists(ref text, offset));
            tokens.AddRange(ParseCodeBlocks(ref text, offset));
            tokens.AddRange(ParseBlockQuotes(ref text, offset));

            // And, finally, the most important one: splitting into lines and parsing out the spans
            tokens.AddRange(ParseSpans(ref text, offset));

            return tokens;
        }

        /// <summary>
        /// Determine if the given paragraph of text contains any multi-line tokens.
        /// </summary>
        public static bool ParagraphContainsMultilineTokens(string text)
        {
            // For now, just look for H1/H2 defined by ---- or ====.
            if (Markdown.HeaderSetextRegex.IsMatch(text))
                return true;

            return false;
        }

        /// <summary>
        /// Parse a text snapshot into markdown sections.  This is only part of the markdown parser that is really aware of the editor,
        /// but it keeps us from re-creating all the "GetLineFromLineNumber"-type methods.
        /// </summary>
        /// <param name="snapshot"></param>
        /// <returns></returns>
        public static IEnumerable<Token> ParseMarkdownSections(ITextSnapshot snapshot)
        {
            string text = snapshot.GetText();

            List<Tuple<int, TokenType>> startPoints =
                   new List<Tuple<int, TokenType>>(ParseMarkdownParagraph(text).Where(t => IsHeaderToken(t))
                                                                               .Select(t => Tuple.Create(t.Span.Start, t.TokenType)));

            List<Token> sections = new List<Token>();
            Stack<Tuple<int, TokenType>> regions = new Stack<Tuple<int, TokenType>>();

            foreach (var start in startPoints)
            {
                int previousLineNumber = Math.Max(0, snapshot.GetLineNumberFromPosition(start.Item1) - 1);
                int end = snapshot.GetLineFromLineNumber(previousLineNumber).End;

                while (regions.Count > 0 && regions.Peek().Item2 >= start.Item2)
                {
                    var region = regions.Pop();
                    var span = Span.FromBounds(region.Item1, end);
                    sections.Add(new Token(region.Item2, span));
                }

                regions.Push(start);
            }

            while (regions.Count > 0)
            {
                var region = regions.Pop();
                var span = Span.FromBounds(region.Item1, snapshot.Length);
                sections.Add(new Token(region.Item2, span));
            }

            sections.Sort((left, right) =>
                {
                    if (left.Span.Start != right.Span.Start)
                        return left.Span.Start.CompareTo(right.Span.Start);

                    return right.Span.Length.CompareTo(left.Span.Length);
                });

            return sections;
        }
        
        /// <summary>
        /// Markdown token types.
        /// </summary>
        public enum TokenType
        {
            // Bold/italics
            Italics,
            Bold,
            
            // Headers
            H1, H2, H3, H4, H5, H6,

            // Lists
            UnorderedListElement,
            OrderedListElement,

            // Code/pre
            PreBlock,
            CodeBlock,

            // Quotes
            Blockquote,

            // Links
            LinkExpression,
            LinkText,
            LinkTitle,
            LinkLabel,

            // Link URLs
            InlineUrl,
            UrlDefinition,
            AutomaticUrl,
            
            // Images
            ImageExpression,
            ImageAltText,
            ImageTitle,
            ImageLabel,

            // Miscellaneous
            HorizontalRule,
        }

        /// <summary>
        /// A Markdown token, which is a Span in the given text and an associated token type.
        /// </summary>
        public struct Token
        {
            public Token(TokenType type, Span span) { TokenType = type; Span = span; }

            public TokenType TokenType;
            public Span Span;
        }

        #endregion

        #region Markdown public validation interface

        ///////////////////////////////////////////////////////////////////////////////////
        //
        // Validate the mardown directive syntax according to the ruleset definitions
        //
        // Copyright (c) 2014 Microsoft Corporation.
        // Author: Junyi Yi (junyi@microsoft.com) - Initial version
        //
        ///////////////////////////////////////////////////////////////////////////////////

        /// <summary>
        /// Validate the whole document according to the specified ruleset.
        /// </summary>
        /// <param name="snapshot">The whole document snapshot.</param>
        /// <param name="errorTagger">The tagger used to generate error squiggles.</param>
        /// <param name="ruleset">The specified ruleset.</param>
        public static void ValidateDirectiveSyntax(ITextSnapshot snapshot, DirectiveRuleset ruleset, SimpleTagger<ErrorTag> errorTagger)
        {
            // Remove all current error squiggles
            errorTagger.RemoveTagSpans(errorTagSpan => true);

            // Get the full document text and clear all HTML tags
            string text = snapshot.GetText();
            text = MarkdownParser.DestroyHtmlTags(text);

            // Three cases: 
            // 0123456789              01234567 8              01234567  8
            // [  WA ab ]              [  WA ab \n             [  WA ab EOT
            // |        |-endIndex=9   |        |-endIndex=8   |         |-endIndex=8
            // |-startIndex=0          |-startIndex=0          |-startIndex=0
            
            // Greedily search for the pair of '[...]' (supports nested pair '[... [...] ...]')
            // Here 'Greedily' means if we have a string '[...[...]', it would also treat the latter '[...]' as the pair
            for (int startIndex = text.IndexOf('['); startIndex >= 0; startIndex = text.IndexOf('[', startIndex))
            {
                int endIndex = MarkdownParser.FindCorrespondingEndBracket(text, startIndex + 1);

                // Get the directive content string
                ITrackingSpan overallDirective = snapshot.CreateTrackingSpan(startIndex + 1, endIndex - startIndex - 1, SpanTrackingMode.EdgeInclusive);
                string directive = overallDirective.GetText(snapshot);
                var directiveMatches = Regex.Matches(directive, string.Concat(@"^\s*(", ValidationUtilities.DirectiveNameRegularPattern, @")(.*)$"));
                if (directiveMatches.Count != 1 || !directiveMatches[0].Success || directiveMatches[0].Groups.Count != 3 || directiveMatches[0].Value != directive)
                {
                    startIndex++;
                    continue;
                }
                string directiveName = directiveMatches[0].Groups[1].Value;
                string directiveContent = directiveMatches[0].Groups[2].Value;

                var rule = ruleset.TryGetDirectiveRule(directiveName);
                if (rule != null)
                {
                    // Get the preceding and following directive string of the same line
                    ITextSnapshotLine line = snapshot.GetLineFromPosition(startIndex);
                    string precedingText = snapshot.GetText(line.Start, startIndex - line.Start);
                    string followingText = endIndex < line.End ? snapshot.GetText(endIndex + 1, line.End - endIndex - 1) : string.Empty;

                    // If we found a exactly-matched rule, just validate it
                    string message = rule.Validate(directiveContent, precedingText, followingText);
                    if (message != null)
                    {
                        ITrackingSpan squiggleSpan = overallDirective;
                        if (rule.SquiggleWholeLine)
                        {
                            squiggleSpan = snapshot.CreateTrackingSpan(line.Start, line.Length, SpanTrackingMode.EdgeInclusive);
                        }
                        errorTagger.CreateTagSpan(squiggleSpan, new ErrorTag(PredefinedErrorTypeNames.SyntaxError, message));
                    }

                    // If we miss the closing bracket, give out the prompt message
                    if (endIndex >= text.Length || text[endIndex] != ']')
                    {
                        errorTagger.CreateTagSpan(snapshot.CreateTrackingSpan(line.End, 0, SpanTrackingMode.EdgePositive), new ErrorTag(PredefinedErrorTypeNames.CompilerError, "Missing the closing bracket"));
                    }
                }
                else
                {
                    // Otherwise we may take a look at the suspects
                    var suspects = ruleset.GetSuspects(directive);
                    if (suspects.Count() > 0)
                    {
                        StringBuilder suspectPrompt = new StringBuilder();
                        suspectPrompt.AppendLine("Are you trying to enter one of the following directives?");
                        foreach (var suspect in suspects)
                        {
                            suspectPrompt.AppendLine(string.Format("    \u2022 {0} - {1}", suspect.ParentRule.DirectiveName, suspect.SuggestionMessage));
                        }
                        errorTagger.CreateTagSpan(overallDirective, new ErrorTag(PredefinedErrorTypeNames.Warning, suspectPrompt.ToString().TrimEnd()));
                    }
                }

                startIndex = endIndex;
            }
        }

        /// <summary>
        /// Finds the ending bracket ']' in the text corresponding to the start bracket '[' within the same line. This methods supports nested pair.
        /// </summary>
        /// <param name="text">The specified text.</param>
        /// <param name="startIndex">The specified index.</param>
        /// <returns>The ending bracket ']' character index or EndOfLine or EndOfText.</returns>
        private static int FindCorrespondingEndBracket(string text, int startIndex)
        {
            int nestLevel = 0;  // Currently no '[...]' pair was nested
            for (; startIndex < text.Length; startIndex++)
            {
                switch (text[startIndex])
                {
                    case '[':
                        nestLevel++;
                        break;
                    case ']':
                        if (nestLevel-- == 0)
                            return startIndex;
                        break;
                    case '\r':
                    case '\n':
                        return startIndex;
                }
            }
            return text.Length;
        }

        #endregion Markdown public validation interface

        #region Parser methods (private)

        static IEnumerable<Token> ParseSpans(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            tokens.AddRange(ParseCodeSpans(ref text, offset));

            // Make sure we don't parse backslash-escaped pieces of the text
            text = DestroyBackslashEscapes(text);

            tokens.AddRange(ParseImages(ref text, offset));
            tokens.AddRange(ParseAnchors(ref text, offset));
            tokens.AddRange(ParseItalicsAndBold(ref text, offset));

            return tokens;
        }

        static IEnumerable<Token> ParseCodeBlocks(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = ParserCodeBlockRegex.Replace(text, match =>
                {
                    tokens.Add(new Token(TokenType.CodeBlock, SpanFromGroup(match.Groups[1])));

                    return DestroyMarkdownCharsInBlock(match.Value);
                });

            return tokens;
        }

        static IEnumerable<Token> ParseCodeSpans(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.CodeSpanRegex.Replace(text, match =>
            {
                tokens.Add(new Token(TokenType.CodeBlock, SpanFromGroup(match.Groups[2], offset)));
                return DestroyMarkdownCharsInBlock(match.Value);
            });

            return tokens;
        }

        static IEnumerable<Token> ParseHorizontalRules(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.HorizontalRulesRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.HorizontalRule, SpanFromGroup(match.Groups[0])));
                });

            return tokens;
        }

        static IEnumerable<Token> ParseBlockQuotes(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.BlockquoteRegex.ReplaceWithDummy(text, match =>
                {
                    string bq = match.Groups[1].Value;
                    int bqOffset = match.Groups[1].Index;

                    tokens.Add(new Token(TokenType.Blockquote, SpanFromGroup(match.Groups[1])));

                    // This is kinda rough - we're going to trim each line and re-parse them as paragraphs
                    foreach (var line in Markdown._entireLines.Matches(bq).Cast<Match>())
                    {
                        string entireLine = line.Value;
                        Match strip = Regex.Match(entireLine, @"^[ \t]*>[ \t]?(.*)$", RegexOptions.Singleline);

                        if (strip.Success)
                        {
                            string toParse = strip.Groups[1].Value;
                            int toParseOffset = line.Index + strip.Groups[1].Index + bqOffset + offset;

                            tokens.AddRange(ParseMarkdownParagraph(toParse, toParseOffset));
                        }
                    }
                });

            return tokens;
        }

        static IEnumerable<Token> ParseHeaders(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.HeaderSetextRegex.ReplaceWithDummy(text, match =>
                {
                    TokenType type = match.Groups[2].Value.StartsWith("=") ? TokenType.H1 : TokenType.H2;
                    tokens.Add(new Token(type, SpanFromGroup(match.Groups[0], offset)));

                    string headerText = match.Groups[1].Value;
                    tokens.AddRange(ParseSpans(ref headerText, match.Groups[1].Index + offset));
                });

            text = Markdown.HeaderAtxRegex.ReplaceWithDummy(text, match =>
                {
                    TokenType type;
                    switch (match.Groups[1].Value.Length)
                    {
                        case 1: type = TokenType.H1; break;
                        case 2: type = TokenType.H2; break;
                        case 3: type = TokenType.H3; break;
                        case 4: type = TokenType.H4; break;
                        case 5: type = TokenType.H5; break;
                        case 6: type = TokenType.H6; break;
                        default:
                            throw new ApplicationException("The HeaderAtxRegex Regex produced an impossible match.");
                    }

                    tokens.Add(new Token(type, SpanFromGroup(match.Groups[0], offset)));

                    string headerText = match.Groups[2].Value;
                    tokens.AddRange(ParseSpans(ref headerText, match.Groups[2].Index + offset));
                });

            return tokens;
        }

        static IEnumerable<Token> ParseLists(ref string text, int offset = 0, int listLevel = 0)
        {
            List<Token> tokens = new List<Token>();

            Regex regex = (listLevel == 0) ? ParserListTopLevelRegex : Markdown.ListNestedRegex;

            text = regex.Replace(text, match =>
                {
                    TokenType type = Regex.IsMatch(match.Groups[3].Value, Markdown.MarkerUL) ? 
                                     TokenType.UnorderedListElement : TokenType.OrderedListElement;

                    string list = match.Groups[1].Value;

                    int oldTokenCount = tokens.Count;

                    tokens.AddRange(ParseListItems(ref list, listLevel, type, match.Groups[1].Index + offset));

                    // If we didn't add anything, *don't do a replace*.  Leave this row for other things to parse.
                    if (oldTokenCount == tokens.Count)
                        return match.Value;
                    else
                        return new string(_dummyChar, match.Length);
                });

            return tokens;
        }

        static IEnumerable<Token> ParseListItems(ref string list, int listLevel, TokenType listType, int offset = 0)
        {
            Regex regex = (listType == TokenType.OrderedListElement) ? OlListItemRegex : UlListItemRegex;

            List<Token> tokens = new List<Token>();

            list = regex.ReplaceWithDummy(list, match =>
                {
                    // Add a token for the list bullet (like * or 1.)
                    tokens.Add(new Token(listType, SpanFromGroup(match.Groups[3], offset)));

                    string item = match.Groups[4].Value;
                    string leadingLine = match.Groups[1].Value;

                    int matchOffset = match.Groups[4].Index + offset;

                    if (!String.IsNullOrEmpty(leadingLine) || Regex.IsMatch(item, @"\n{2,}"))
                    {
                        // This is kinda rough - we're going to trim each line and re-parse them as paragraphs.
                        // This should work for everything but the two-line header format, which we can't have here anyways (I don't think)
                        foreach (var line in Markdown._entireLines.Matches(item).Cast<Match>())
                        {
                            Match strip = Regex.Match(line.Value, @"^(\t|[ ]{1," + Markdown.TabWidth + @"})?(.*)$", RegexOptions.Singleline);

                            if (strip.Success)
                            {
                                string toParse = strip.Groups[2].Value;
                                int toParseOffset = matchOffset + strip.Groups[1].Index;

                                tokens.AddRange(ParseMarkdownParagraph(toParse, toParseOffset));
                            }
                        }
                    }
                    else
                    {
                        // recursion for sub-lists
                        tokens.AddRange(ParseLists(ref item, matchOffset, listLevel + 1));

                        // Just in case, parse spans on the segment as well.  If ParseLists found sublists, it would
                        // have blanked them out, so this won't do anything in that case.  If there weren't sublists,
                        // though, this will take care of subexpressions.
                        tokens.AddRange(ParseSpans(ref item, matchOffset));
                    }
                });
            
            return tokens;
        }

        static IEnumerable<Token> ParseImages(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.ImagesRefRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.ImageExpression, SpanFromGroup(match.Groups[1], offset)));
                    tokens.Add(new Token(TokenType.ImageAltText, SpanFromGroup(match.Groups[2], offset)));
                    tokens.Add(new Token(TokenType.ImageLabel, SpanFromGroup(match.Groups[3], offset)));
                });

            text = Markdown.ImagesInlineRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.ImageExpression, SpanFromGroup(match.Groups[1], offset)));
                    tokens.Add(new Token(TokenType.ImageAltText, SpanFromGroup(match.Groups[2], offset)));
                    tokens.Add(new Token(TokenType.InlineUrl, SpanFromGroup(match.Groups[3], offset)));
                    tokens.Add(new Token(TokenType.ImageTitle, SpanFromGroup(match.Groups[6], offset)));
                });

            return tokens;
        }

        static IEnumerable<Token> ParseAnchors(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            // First, handle reference-style links: [link text] [id]
            text = Markdown.AnchorRefRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.LinkExpression, SpanFromGroup(match.Groups[0], offset)));
                    tokens.Add(new Token(TokenType.LinkText, SpanFromGroup(match.Groups[2], offset)));
                    tokens.Add(new Token(TokenType.LinkLabel, SpanFromGroup(match.Groups[3], offset)));
                });
            // Next, inline-style links: [link text](url "optional title") or [link text](url "optional title")
            text = Markdown.AnchorInlineRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.LinkExpression, SpanFromGroup(match.Groups[0], offset)));
                    tokens.Add(new Token(TokenType.LinkText, SpanFromGroup(match.Groups[2], offset)));
                    tokens.Add(new Token(TokenType.InlineUrl, SpanFromGroup(match.Groups[3], offset)));
                    tokens.Add(new Token(TokenType.LinkTitle, SpanFromGroup(match.Groups[6], offset)));
                });
            //  Last, handle reference-style shortcuts: [link text]
            text = Markdown.AnchorRefShortcutRegex.ReplaceWithDummy(text, match =>
                {
                    tokens.Add(new Token(TokenType.LinkExpression, SpanFromGroup(match.Groups[0], offset)));
                    tokens.Add(new Token(TokenType.LinkText, SpanFromGroup(match.Groups[2], offset)));
                    tokens.Add(new Token(TokenType.LinkLabel, SpanFromGroup(match.Groups[2], offset)));
                });

            return tokens;
        }

        static IEnumerable<Token> ParseItalicsAndBold(ref string text, int offset = 0)
        {
            List<Token> tokens = new List<Token>();

            text = Markdown.BoldRegex.Replace(text, match =>
                {
                    tokens.Add(new Token(TokenType.Bold, SpanFromGroup(match.Groups[2], offset)));
                    return new string(_dummyChar, 2) + match.Groups[2].Value + new string(_dummyChar, 2);
                });

            text = Markdown.ItalicRegex.Replace(text, match =>
                {
                    tokens.Add(new Token(TokenType.Italics, SpanFromGroup(match.Groups[2], offset)));                  
                    return new string(_dummyChar, 3) + match.Groups[2].Value + new string(_dummyChar, 3);
                });

            return tokens;
        }
        
        #endregion

        #region Helpers

        static bool IsHeaderToken(MarkdownParser.Token token)
        {
            return token.TokenType >= MarkdownParser.TokenType.H1 && token.TokenType <= MarkdownParser.TokenType.H6;
        }

        static string DestroyHtmlTags(string text)
        {
            int pos = 0;
            int tagStart = 0;

            StringBuilder newText = new StringBuilder();

            foreach (Match m in Markdown.HtmlTokensRegex.Matches(text))
            {
                tagStart = m.Index;

                if (pos < tagStart)
                    newText.Append(text.Substring(pos, tagStart - pos));

                newText.Append(new string(_dummyChar, m.Length));
                pos = tagStart + m.Length;
            }

            if (pos < text.Length)
                newText.Append(text.Substring(pos, text.Length - pos));

            return newText.ToString();
        }

        static Regex MagicMarkdownCharRegex = new Regex(@"[\*_{}[\]]", RegexOptions.Compiled | RegexOptions.ExplicitCapture | RegexOptions.Compiled);
        static string DestroyMarkdownCharsInBlock(string block)
        {
            // Destroy characters that are magic in markdown
            return MagicMarkdownCharRegex.Replace(block, new string(_dummyChar, 1));
        }

        static string DestroyBackslashEscapes(string text)
        {
            if (!text.Contains('\\'))
                return text;

            // All the backslash strings are two characters long
            string replacement = new string(_dummyChar, 2);

            foreach (var pair in Markdown.BackslashEscapeTable)
                text = text.Replace(pair.Key, replacement);
            return text;
        }

        static Span SpanFromGroup(Group group, int offset = 0)
        {
            return new Span(group.Index + offset, group.Length);
        }

        #endregion
    }
}
