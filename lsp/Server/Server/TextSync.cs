using LspServer;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Server
{
    class TextSync
    {

        public static List<string> ApplyChange(in List<string> inputLines, FileChange fileChange)
        {
            List<string> returnedLines = inputLines;
            // Break up the change lines
            string[] changeLines = fileChange.InsertString.Split('\n');

            if (fileChange.IsReload)
            {
                returnedLines.Clear();
                foreach (var changeLine in changeLines)
                {
                    returnedLines.Add(changeLine);
                }
            }
            else
            {
                // VSCode sometimes likes to give the change start line as (FileLines.Count + 1).
                // This used to crash EditorServices, but we now treat it as an append.
                // See https://github.com/PowerShell/vscode-powershell/issues/1283
                if (fileChange.Line == inputLines.Count + 1)
                {
                    foreach (string addedLine in changeLines)
                    {
                        string finalLine = addedLine.TrimEnd('\r');
                        returnedLines.Add(finalLine);
                    }
                }
                // Similarly, when lines are deleted from the end of the file,
                // VSCode likes to give the end line as (FileLines.Count + 1).
                else if (fileChange.EndLine == inputLines.Count + 1 && String.Empty.Equals(fileChange.InsertString))
                {
                    int lineIndex = fileChange.Line - 1;
                    returnedLines.RemoveRange(lineIndex, inputLines.Count - lineIndex);
                }
                // Otherwise, the change needs to go between existing content
                else
                {
                    ValidatePosition(inputLines, fileChange.Line, fileChange.Offset);
                    ValidatePosition(inputLines, fileChange.EndLine, fileChange.EndOffset);

                    // Get the first fragment of the first line
                    string firstLineFragment =
                    returnedLines[fileChange.Line - 1]
                        .Substring(0, fileChange.Offset - 1);

                    // Get the last fragment of the last line
                    string endLine = returnedLines[fileChange.EndLine - 1];
                    string lastLineFragment =
                    endLine.Substring(
                        fileChange.EndOffset - 1,
                        (returnedLines[fileChange.EndLine - 1].Length - fileChange.EndOffset) + 1);

                    // Remove the old lines
                    for (int i = 0; i <= fileChange.EndLine - fileChange.Line; i++)
                    {
                        returnedLines.RemoveAt(fileChange.Line - 1);
                    }

                    // Build and insert the new lines
                    int currentLineNumber = fileChange.Line;
                    for (int changeIndex = 0; changeIndex < changeLines.Length; changeIndex++)
                    {
                        // Since we split the lines above using \n, make sure to
                        // trim the ending \r's off as well.
                        string finalLine = changeLines[changeIndex].TrimEnd('\r');

                        // Should we add first or last line fragments?
                        if (changeIndex == 0)
                        {
                            // Append the first line fragment
                            finalLine = firstLineFragment + finalLine;
                        }
                        if (changeIndex == changeLines.Length - 1)
                        {
                            // Append the last line fragment
                            finalLine = finalLine + lastLineFragment;
                        }

                        returnedLines.Insert(currentLineNumber - 1, finalLine);
                        currentLineNumber++;
                    }
                }
            }
            return returnedLines;

            // Parse the script again to be up-to-date
            //this.ParseFileContents();
        }


        /// <summary>
        /// Throws ArgumentOutOfRangeException if the given position is outside
        /// of the file's buffer extents.
        /// </summary>
        /// <param name="line">The 1-based line to be validated.</param>
        /// <param name="column">The 1-based column to be validated.</param>
        public static void ValidatePosition(in List<string> inputLines, int line, int column)
        {
            int maxLine = inputLines.Count;
            if (line < 1 || line > maxLine)
            {
                throw new ArgumentOutOfRangeException($"Position {line}:{column} is outside of the line range of 1 to {maxLine}.");
            }

            // The maximum column is either **one past** the length of the string
            // or 1 if the string is empty.
            string lineString = inputLines[line - 1];
            int maxColumn = lineString.Length > 0 ? lineString.Length + 1 : 1;

            if (column < 1 || column > maxColumn)
            {
                throw new ArgumentOutOfRangeException($"Position {line}:{column} is outside of the column range of 1 to {maxColumn}.");
            }
        }

    }
}
