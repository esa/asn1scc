using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr.Runtime.Tree;
using Antlr.Runtime;
using Antlr.Asn1;
using Antlr.Acn;
using System.IO;

namespace Antlr
{
    public class Comment
    {
        public static string[] GetComments(IToken[] FileTokens, List<IToken> alreadyTakenComments, int lastTokenLineNo,
            int prevTokenIndex, int nextTokenIndex, bool acn = false)
        {
            List<string> comments = new List<string>();
            int WS = acn ? acnLexer.WS : asn1Lexer.WS;
            int COMMENT = acn ? acnLexer.COMMENT : asn1Lexer.COMMENT;
            int COMMENT2 = acn ? acnLexer.COMMENT2 : asn1Lexer.COMMENT2;

            //first see if there comments on the same line

            while (nextTokenIndex >= 0 && nextTokenIndex < FileTokens.Length)
            {
                IToken t = FileTokens[nextTokenIndex++];
                if (alreadyTakenComments.Contains(t))
                {
                    break;
                }
                if (t.Line != lastTokenLineNo)
                {
                    break;
                }
                if (t.Type == WS)
                {
                    continue;
                }
                else if (t.Type == COMMENT || t.Type == COMMENT2)
                {
                    if (!t.Text.StartsWith("--!"))
                        comments.Insert(0, t.Type == COMMENT2 && t.Text.Length >=2 ? t.Text.Substring(2).TrimEnd() : t.Text);
                    alreadyTakenComments.Add(t);
                }
                else
                {
                    break;
                }

            }

            //if no comments were found at the same line, then look back (above)
            if (comments.Count == 0)
            {

                while (prevTokenIndex >= 0 && prevTokenIndex < FileTokens.Length)
                {
                    IToken t = FileTokens[prevTokenIndex--];
                    if (alreadyTakenComments.Contains(t))
                        break;
                    if (t.Type == WS)
                        continue;
                    else if (t.Type == COMMENT || t.Type == COMMENT2)
                    {
                        if (!t.Text.StartsWith("--!"))
                            comments.Insert(0, t.Type == COMMENT2 && t.Text.Length >= 2 ? t.Text.Substring(2).TrimEnd() : t.Text);
                        alreadyTakenComments.Add(t);
                    }
                    else
                        break;
                }
            }

            return comments.ToArray();
        }
    }
}
