import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

public class Day4 {
    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.err.println("Expected: <filename>");
            System.exit(1);
        }
        String filename = args[0];
        File file = new File(filename);
        BufferedReader in = new BufferedReader(new FileReader(file));
        List<Pair<Range, Range>> pairs = in.lines().map(Day4::parseLine).collect(Collectors.toList());
        System.out.println("Enclosing ranges = " + enclosingPairs(pairs));
        System.out.println("Overlapping ranges = " + overlappingPairs(pairs));
        in.close();
    }

    private static Pair<Range, Range> parseLine(String line) {
        String[] parts = line.split(",");
        if (parts.length != 2)
            throw new InvalidLineFormat(String.format("Line format must be <range>,<range> but found \"%s\"", line));
        Range first = Range.parse(parts[0]);
        Range second = Range.parse(parts[1]);
        return new Pair<>(first, second);
    }

    private static int enclosingPairs(List<Pair<Range, Range>> pairs) {
        int sum = 0;
        for (Pair<Range, Range> pair : pairs) {
            Range first = pair.first();
            Range second = pair.second();
            if (first.encloses(second) || second.encloses(first))
                ++sum;
        }
        return sum;
    }

    private static int overlappingPairs(List<Pair<Range, Range>> pairs) {
        int sum = 0;
        for (Pair<Range, Range> pair : pairs) {
            Range first = pair.first();
            Range second = pair.second();
            if (first.overlaps(second))
                ++sum;
        }
        return sum;
    }
}

class Range {
    private int start;
    private int end;

    public Range(int start, int end) {
        if (start > end)
            throw new InvalidRangeException(
                    String.format("Invalid range: start %d is greater that end %d", start, end));
        this.start = start;
        this.end = end;
    }

    public boolean encloses(Range ot) {
        return this.start <= ot.start && this.end >= ot.end;
    }

    public boolean overlaps(Range ot) {
        return (this.start <= ot.end && this.end >= ot.end) || (this.end >= ot.start && this.end <= ot.end);
    }

    public static Range parse(String str) {
        String[] parts = str.split("-");
        if (parts.length != 2)
            throw new InvalidRangeException(String.format("Too many dashes in range string: %s", str));
        int start = Integer.parseInt(parts[0]);
        int end = Integer.parseInt(parts[1]);
        return new Range(start, end);
    }
}

class Pair<T, U> {
    private T first;
    private U second;

    public Pair(T first, U second) {
        this.first = first;
        this.second = second;
    }

    public T first() {
        return first;
    }

    public U second() {
        return second;
    }
}

class InvalidRangeException extends RuntimeException {
    public InvalidRangeException(String msg) {
        super(msg);
    }
}

class InvalidLineFormat extends RuntimeException {
    public InvalidLineFormat(String msg) {
        super(msg);
    }
}