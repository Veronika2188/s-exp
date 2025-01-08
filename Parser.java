import java.util.ArrayList;
import java.util.List;

class SExp {
    @Override
    public String toString() {
        return "SExp";
    }
}

class Atom extends SExp {
    private String value;

    public Atom(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }
}

class SList extends SExp {
    private List<SExp> children;

    public SList(List<SExp> children) {
        this.children = children;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("(");
        for (SExp child : children) {
            sb.append(child.toString());
        }
        sb.append(")");
        return sb.toString();
    }
}

public class Parser {
    public static SExp parse(String input) {
        List<String> tokens = tokenize(input);
        return parseTokens(tokens);
    }

    private static List<String> tokenize(String input) {
        String stringWithSpaces = input.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ");
        String[] tokensArray = stringWithSpaces.trim().split("\\s+");
        List<String> tokens = new ArrayList<>();
        for (String token : tokensArray) {
            if (!token.isEmpty()) {
                tokens.add(token);
            }
        }
        return tokens;
    }

    private static SExp parseTokens(List<String> tokens) {
        if (tokens.isEmpty()) {
            throw new IllegalArgumentException("Unexpected end of input");
        }

        String token = tokens.remove(0);
        if (token.equals("(")) {
            List<SExp> children = new ArrayList<>();
            while (!tokens.isEmpty() && !tokens.get(0).equals(")")) {
                children.add(parseTokens(tokens));
            }
            if (tokens.isEmpty()) {
                throw new IllegalArgumentException("Unmatched (");
            }
            tokens.remove(0); // Remove ')'
            return new SList(children);
        } else if (token.equals(")")) {
            throw new IllegalArgumentException("Unmatched )");
        } else {
            return new Atom(token);
        }
    }

    public static void main(String[] args) {
        String input = "(+ 1 (* 2 3))";
        SExp result = parse(input);
        System.out.println(result.toString());
    }
}
