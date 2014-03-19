using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;

namespace MarkdownMode
{
    internal class MarkdownBackgroundParser : BackgroundParser
    {
        public MarkdownBackgroundParser(ITextBuffer textBuffer, TaskScheduler taskScheduler, ITextDocumentFactoryService textDocumentFactoryService)
            : base(textBuffer, taskScheduler, textDocumentFactoryService)
        {
            ReparseDelay = TimeSpan.FromMilliseconds(300);

            ITextDocument document;
            if (textDocumentFactoryService.TryGetTextDocument(textBuffer, out document))
                this.LoadRuleset(document);
        }

        protected override void ReParseImpl()
        {
            ITextSnapshot snapshot = TextBuffer.CurrentSnapshot;
            Stopwatch stopwatch = Stopwatch.StartNew();

            List<MarkdownSection> sections = new List<MarkdownSection>(
                MarkdownParser.ParseMarkdownSections(snapshot)
                              .Select(t => new MarkdownSection()
                              {
                                  TokenType = t.TokenType,
                                  Span = snapshot.CreateTrackingSpan(t.Span, SpanTrackingMode.EdgeExclusive)
                              }));

            OnParseComplete(new MarkdownParseResultEventArgs(sections, snapshot, stopwatch.Elapsed));
            
            this.ReValidateSyntax();
        }

        #region Markdown Directive Syntax Validation

        //////////////////////////////////////////////////////////////////////
        //
        // Validate the mardown syntax according to the ruleset definitions
        //
        // Copyright (c) 2014 Microsoft Corporation.
        // Author: Junyi Yi (junyi@microsoft.com) - Initial version
        //
        //////////////////////////////////////////////////////////////////////


        private DirectiveRuleset ruleset = null;

        public string RulesetFilePath { get; private set; }
        public string NoRulesetFileReason { get; private set; }

        /// <summary>
        /// Load the ruleset file according to the document file path.
        /// </summary>
        /// <param name="document">The specified document file.</param>
        private void LoadRuleset(ITextDocument document)
        {
            try
            {
                for (string path = Path.GetDirectoryName(document.FilePath); path != null; path = Path.GetDirectoryName(path))
                {
                    string[] rulesetFiles = Directory.GetFiles(path, "*.ruleset");
                    if (rulesetFiles.Length == 1)
                    {
                        this.ruleset = DirectiveRuleset.LoadFromRulesetFile(rulesetFiles[0]);
                        this.RulesetFilePath = Path.GetFullPath(rulesetFiles[0]);
                        this.NoRulesetFileReason = null;
                    }
                    else if (rulesetFiles.Length > 1)
                    {
                        this.RulesetFilePath = null;
                        this.NoRulesetFileReason = string.Format("Folder \"{0}\" contains more than one \"*.ruleset\" files", path);
                    }
                    if (rulesetFiles.Length > 0)
                        break;
                }
                if (this.ruleset == null)
                    this.NoRulesetFileReason = "No \"*.ruleset\" file was found in the directory hierarchy";
            }
            catch (Exception ex)
            {
                this.RulesetFilePath = null;
                this.NoRulesetFileReason = ex.ToString();
            }
        }

        /// <summary>
        /// Revalidate the whole mardown syntax according to the ruleset definitions.
        /// </summary>
        private void ReValidateSyntax()
        {
            if (this.ruleset != null)
            {
                var errorTagger = base.TextBuffer.Properties.GetOrCreateSingletonProperty(() => new SimpleTagger<ErrorTag>(base.TextBuffer));
                MarkdownParser.ValidateDirectiveSyntax(base.TextBuffer.CurrentSnapshot, this.ruleset, errorTagger);
            }
        }

        #endregion Markdown Directive Syntax Validation

    }
}
