using System;
using System.Collections.Generic;
using System.Linq;

public abstract class SExp
{
    public override string ToString()
    {
        return "SExp";
    }
}

public class Atom : SExp
{
    public string Value { get; }

    public Atom(string value)
    {
        Value = value;
    }

    public override string ToString()
    {
        return Value;
    }
}

public class SList : SExp
{
    public List<SExp> Children { get; }

    public SList(List<SExp> children)
    {
        Children = children;
    }

    public override string ToString()
    {
        return "(" + string.Join("", Children.Select(child => child.ToString())) + ")";
    }
}

public class Parser
{
    public static SExp Parse(string input)
    {
        var tokens = Tokenize(input);
        return ParseTokens(tokens);
    }

    private static List<string> Tokenize(string input)
    {
        return input.Replace("(", " ( ").Replace(")", " ) ").Split(" ", StringSplitOptions.RemoveEmptyEntries).ToList();
    }

    private static SExp ParseTokens(List<string> tokens)
    {
        if (tokens.Count == 0)
        {
            throw new ArgumentException("Unexpected end of input");
        }

        var token = tokens[0];
        tokens.RemoveAt(0);
        if (token == "(")
        {
            var children = new List<SExp>();
            while (tokens.Count > 0 && tokens[0] != ")")
            {
                children.Add(ParseTokens(tokens));
            }
            if (tokens.Count == 0)
            {
                throw new ArgumentException("Unmatched (");
            }
            tokens.RemoveAt(0); // Remove ")"
            return new SList(children);
        }
        else if (token == ")")
        {
            throw new ArgumentException("Unmatched )");
        }
        else
        {
            return new Atom(token);
        }
    }

    public static void Main(string[] args)
    {
        string input = "(+ 1 (* 2 3))";
        SExp result = Parse(input);
        Console.WriteLine(result);
    }
}
