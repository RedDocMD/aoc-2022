const std = @import("std");
const heap = std.heap;
const process = std.process;
const fs = std.fs;
const math = std.math;
const mem = std.mem;
const fmt = std.fmt;
const sdprint = std.debug.print;

pub fn main() !void {
    var alloc = heap.ArenaAllocator.init(heap.page_allocator);
    defer alloc.deinit();

    var args_iter = try process.argsWithAllocator(alloc.allocator());
    var args = std.ArrayList([]const u8).init(alloc.allocator());
    while (args_iter.next()) |arg| {
        try args.append(arg);
    }

    if (args.items.len != 2) {
        sdprint("Expected: <arg>\n", .{});
        process.exit(1);
    }

    const filename = args.items[1];
    const cwd = fs.cwd();
    const openFlags = fs.File.OpenFlags{ .mode = fs.File.OpenMode.read_only };
    const file = try cwd.openFile(filename, openFlags);
    const contents = try file.reader().readAllAlloc(alloc.allocator(), math.maxInt(usize));

    var input = try parseInput(contents, alloc.allocator());
    sdprint("{} stacks and {} moves\n", .{ input.stacks.len, input.moves.len });
    for (input.stacks) |stack, i| {
        sdprint("Stack {}: ", .{i + 1});
        stack.print();
    }

    for (input.moves) |move| {
        var from = &input.stacks[move.from - 1];
        var to = &input.stacks[move.to - 1];
        var removed = try from.remove(move.cnt);
        mem.reverse(u8, removed);
        try to.append(removed);
    }
    sdprint("Lasts: ", .{});
    for (input.stacks) |stack| {
        sdprint("{c}", .{stack.last()});
    }
    sdprint("\n", .{});
    for (input.stacks) |stack, i| {
        sdprint("Stack {}: ", .{i + 1});
        stack.print();
    }
}

fn parseInput(input: []const u8, alloc: mem.Allocator) !Input {
    var lineIter = mem.splitBackwards(u8, input, "\n");

    var moves = std.ArrayList(Move).init(alloc);
    _ = lineIter.next();
    while (lineIter.next()) |line| {
        if (line.len == 0)
            break;
        const move = try Move.parse(line);
        try moves.append(move);
    }
    var movesSlice = try moves.toOwnedSlice();
    mem.reverse(Move, movesSlice);

    const bottomLine = lineIter.next() orelse unreachable;
    const stackCnt = parseLastLine(bottomLine);
    var stacks = try std.ArrayList(Stack).initCapacity(alloc, stackCnt);
    stacks.appendNTimesAssumeCapacity(Stack.init(alloc), stackCnt);

    while (lineIter.next()) |line| {
        var i: usize = 0;
        var idx: usize = 0;
        while (i < line.len) {
            if (line[i] != ' ') {
                try stacks.items[idx].push(line[i + 1]);
            }
            idx += 1;
            i += 4;
        }
    }

    return .{
        .stacks = try stacks.toOwnedSlice(),
        .moves = movesSlice,
    };
}

fn parseLastLine(line: []const u8) usize {
    var cnt: usize = 0;
    var iter = mem.tokenize(u8, line, " ");
    while (iter.next()) |_| {
        cnt += 1;
    }
    return cnt;
}

const Input = struct {
    stacks: []Stack,
    moves: []Move,
};

const Stack = struct {
    items: std.ArrayList(u8),

    fn init(alloc: mem.Allocator) Stack {
        return .{ .items = std.ArrayList(u8).init(alloc) };
    }

    fn push(self: *Stack, item: u8) !void {
        return self.items.append(item);
    }

    fn remove(self: *Stack, count: usize) ![]u8 {
        var removed = try std.ArrayList(u8).initCapacity(self.items.allocator, count);
        var i: usize = 0;
        while (i < count) {
            const el = self.items.pop();
            try removed.append(el);
            i += 1;
        }
        return removed.toOwnedSlice();
    }

    fn append(self: *Stack, items: []const u8) !void {
        for (items) |item| {
            try self.items.append(item);
        }
    }

    fn print(self: *const Stack) void {
        sdprint("[", .{});
        for (self.items.items) |item, i| {
            if (i > 0) {
                sdprint(" ", .{});
            }
            sdprint("{c}", .{item});
        }
        sdprint("]\n", .{});
    }

    fn last(self: *const Stack) u8 {
        return self.items.items[self.items.items.len - 1];
    }
};

const Move = struct {
    cnt: usize,
    from: usize,
    to: usize,

    fn parse(line: []const u8) !Move {
        var partsIter = mem.split(u8, line, " ");
        _ = partsIter.next();
        var cntStr = partsIter.next() orelse unreachable;
        _ = partsIter.next();
        var fromStr = partsIter.next() orelse unreachable;
        _ = partsIter.next();
        var toStr = partsIter.next() orelse unreachable;

        return .{
            .cnt = try fmt.parseInt(usize, cntStr, 10),
            .from = try fmt.parseInt(usize, fromStr, 10),
            .to = try fmt.parseInt(usize, toStr, 10),
        };
    }

    fn print(self: *const Move) void {
        sdprint("From = {}, To = {}, Cnt = {}\n", .{ self.from, self.to, self.cnt });
    }
};
