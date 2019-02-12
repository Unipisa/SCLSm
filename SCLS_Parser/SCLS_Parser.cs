using System;
using System.Collections.Generic;
using System.Text;
using SCLS;

namespace SCLS.SCLS_Parser
{
    public class SCLS_Parser
    {
        /// <summary>
        /// Parse an SCLS Simulator initial configuration given by <paramref name="fileName"/>
        /// and put the resulting SCLS.Model in the out paramter <paramref name="result"/> 
        /// </summary>
        /// <param name="fileName">Path of the input file to be parsed</param>
        /// <returns>The resulting SCLS.Model</returns>
        public static Model Parse(string fileName)
        {
            Scanner scanner;
            Parser parser;
            try
            {
            // parse input file
            scanner = new Scanner(fileName);
            parser = new Parser(scanner);
            parser.Parse();
            // check if there are no errors
            Errors E = parser.errors;
            if (E.count == 0) { return Parser.model; }
            else { throw new Exception("Error in parsing"); }
            }
            catch (Exception e) { throw e; }
        }
    }
}
