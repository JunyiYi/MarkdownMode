using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;

namespace MarkdownMode
{
    /// <summary>
    /// This class represents the directive (markdown instructions like "[...]") ruleset collection.
    /// </summary>
    /// <example>
    /// A directive is a string starts with the opening bracket '[', and followed by a directive name (the name contains only lower-letter, UPPER-LETTERS, digit-numbers, dot);
    /// it would end by a closing bracket ']' (also support nested brackets, i.e. the string "[WACOM.INCLUDE [howto]()]" would be treated as a single directive) or EndOfLine.
    /// </example>
    [XmlRoot("DirectiveRuleset", Namespace = ValidationUtilities.ValidationXmlNamespace)]
    public class DirectiveRuleset
    {
        [XmlElement("RulesetName")]
        public string Name { get; set; }

        [XmlArray("ValidationRules")]
        [XmlArrayItem("RegularExpressionRule", typeof(RegExDirectiveRule))]
        public List<DirectiveRule> Rules { get; set; }


        private IDictionary<string, DirectiveRule> ruleMapping = null;

        /// <summary>
        /// Try to retrieve the directive rule according to the specified directive name.
        /// </summary>
        /// <param name="directiveName">The specified directive name.</param>
        /// <returns>The correponding rule of the directive name; <c>null</c> if not found.</returns>
        public DirectiveRule TryGetDirectiveRule(string directiveName)
        {
            DirectiveRule rule;
            if (ruleMapping == null || !ruleMapping.TryGetValue(directiveName, out rule))
                return null;
            return rule;
        }

        /// <summary>
        /// Retrieve all suspects according to the specified directive (exclude the '[' and ']' characters).
        /// </summary>
        /// <param name="directive">The specified directive.</param>
        /// <returns>The list of all suspect directive rules.</returns>
        public IEnumerable<Suspect> GetSuspects(string directive)
        {
            foreach (var rule in this.Rules)
            {
                foreach (var suspect in rule.GetSuspects(directive))
                    yield return suspect;
            }
        }
        
        /// <summary>
        /// Loads the ruleset from a specified "*.ruleset" file.
        /// </summary>
        /// <param name="rulesetFilePath">The specified "*.ruleset" file path.</param>
        /// <returns>The new instance of directive ruleset if success; would throw exception if failed.</returns>
        public static DirectiveRuleset LoadFromRulesetFile(string rulesetFilePath)
        {
            DirectiveRuleset ruleset;
            using (Stream rulesetStream = new FileStream(rulesetFilePath, FileMode.Open))
            {
                ruleset = (DirectiveRuleset) (new XmlSerializer(typeof(DirectiveRuleset))).Deserialize(rulesetStream);
                rulesetStream.Close();
            }
            ruleset.ruleMapping = new Dictionary<string, DirectiveRule>(ruleset.Rules.Count, StringComparer.OrdinalIgnoreCase);
            foreach (var rule in ruleset.Rules)
            {
                ruleset.ruleMapping.Add(rule.DirectiveName, rule);
                foreach (var suspect in rule.Suspects ?? Enumerable.Empty<Suspect>())
                    suspect.ParentRule = rule;
            }
            return ruleset;
        }
    }

    /// <summary>
    /// This class represents the base class of all directive rules.
    /// </summary>
    /// <example>
    /// To use the rule, you must pass the directive content of a directive string to the Validate() method.
    ///     For example, the content of directive string "[WACOM.INCLUDE [howto](http://example.com) ]" is " [howto](http://example.com) ".
    /// The directive name is used to identify different rules.
    ///     For example, the directive name of string "[ WACOM.INCLUDE [howto](http://example.com) ]" is "WACOM.INCLUDE".
    /// </example>
    public abstract class DirectiveRule
    {
        [XmlAttribute("DirectiveName")]
        public string DirectiveName { get; set; }

        [XmlArray("Suspects")]
        [XmlArrayItem("RegularExpressionSuspect", typeof(RegExSuspect))]
        public List<Suspect> Suspects { get; set; }

        [XmlAttribute("SquiggleWholeLine")]
        public bool SquiggleWholeLine { get; set; }

        /// <summary>
        /// Validate the specified directive content string and returns the validation result.
        /// </summary>
        /// <param name="directiveContent">The specified directive content string.</param>
        /// <param name="precedingDirective">The preceding directive content string of the same line.</param>
        /// <param name="followingDirective">The following directive content string of the same line.</param>
        /// <returns><c>null</c> if validation passed; otherwise the error message (allowed to be string.Empty).</returns>
        protected internal abstract string Validate(string directiveContent, string precedingDirective, string followingDirective);

        /// <summary>
        /// Get all suspects of the specified overall directive (exclude the '[' and ']' character) which may be representing the current DirectiveName.
        /// </summary>
        /// <param name="directive">The specified overall directive.</param>
        /// <returns>The list of all suspects.</returns>
        internal IEnumerable<Suspect> GetSuspects(string directive)
        {
            foreach (var suspect in this.Suspects ?? Enumerable.Empty<Suspect>())
                if (suspect.IsSuspectMatch(directive))
                    yield return suspect;
        }
    }

    /// <summary>
    /// This class represents the base class of all suspect rules.
    /// </summary>
    /// <example>
    /// This class is used to guess the user's intension. For example, when the user enters [WACOM.SELECTER ] we may guess that he actually wants to
    /// enter the [WACOM.SELECTOR] directive.
    /// A rule (e.g. [WACOM.SELECTOR] rule) may contains zero or more Suspects (e.g. [WACOM.SELECTER], [WACOM.SELECT]).
    /// </example>
    public abstract class Suspect
    {
        [XmlIgnore]
        public DirectiveRule ParentRule { get; set; }

        [XmlAttribute("SuggestionMessage")]
        public string SuggestionMessage { get; set; }

        /// <summary>
        /// Judge whether the current suspect matches the specified directive.
        /// </summary>
        /// <param name="directive">The specified directive.</param>
        /// <returns><c>true</c> if this suspect mathes the <paramref name="directive"/>; otherwise <c>false</c>.</returns>
        protected internal abstract bool IsSuspectMatch(string directive);
    }

    /// <summary>
    /// This class represents the regular expression validation rule to validate a directive string.
    /// The validation is splited into 3 parts (take the line 'abc [WACOM.INCLUDE (123) ] xyz' as the example):
    ///     * PrecedingSyntax: the pattern used to validate 'abc '
    ///     * Syntax: the pattern used to validate ' (123) '
    ///     * FollowingSyntax: the pattern used to validate ' xyz'
    /// </summary>
    public class RegExDirectiveRule : DirectiveRule
    {
        [XmlElement("Syntax")]
        public string SyntaxPattern { get; set; }

        [XmlElement("PrecedingSyntax")]
        public string PrecedingPattern { get; set; }

        [XmlElement("FollowingSyntax")]
        public string FollowingPattern { get; set; }

        [XmlElement("ErrorMessage")]
        public string ErrorMessage { get; set; }

        protected internal override string Validate(string directiveContent, string precedingDirective, string followingDirective)
        {
            Match match = Regex.Match(directiveContent, this.SyntaxPattern);
            if (!match.Success || match.Value != directiveContent)
                return this.ErrorMessage;
            if (!string.IsNullOrEmpty(this.PrecedingPattern))
            {
                match = Regex.Match(precedingDirective, this.PrecedingPattern);
                if (!match.Success || match.Value != precedingDirective)
                    return this.ErrorMessage;
            }
            if (!string.IsNullOrEmpty(this.FollowingPattern))
            {
                match = Regex.Match(followingDirective, this.FollowingPattern);
                if (!match.Success || match.Value != followingDirective)
                    return this.ErrorMessage;
            }
            return null;
        }
    }

    /// <summary>
    /// This class represents the regular expression suspect rule of a directive.
    /// </summary>
    public class RegExSuspect : Suspect
    {
        [XmlAttribute("Pattern")]
        public string SuspectPattern { get; set; }

        [XmlAttribute("CaseInsensitive")]
        public bool CaseInsensitive { get; set; }

        protected internal override bool IsSuspectMatch(string directive)
        {
            Match match = Regex.Match(directive, this.SuspectPattern, this.CaseInsensitive ? RegexOptions.IgnoreCase : RegexOptions.None);
            if (match.Success)
                return true;
            return false;
        }
    }
}
