using Antlr.Runtime;

namespace Antlr
{
    public static class ErrorFormatter
    {
        private static string FormatErrorHeader(string path, int line, int column, string errType)
        {
            var arr = new object[] { path, ":", line, ":", column, errType };
            return string.Concat(arr);
        }

        public static string GetErrorHeader(string path, RecognitionException e)
        {
            return FormatErrorHeader(path, e.Line, e.CharPositionInLine, ": error:");
        }

        public static string FormatError(string path, int line, int column, string message)
        {
            return FormatErrorHeader(path, line, column, ": error:") + " " + message;
        }

        public static string FormatWarning(string path, int line, int column, string message)
        {
            return FormatErrorHeader(path, line, column, ": warning:") + " " + message;
        }
    }
}
