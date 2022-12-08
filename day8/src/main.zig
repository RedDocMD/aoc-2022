const std = @import("std");
const sdp = std.debug.print;
const heap = std.heap;
const process = std.process;
const fs = std.fs;
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;

pub fn main() !void {
    var alloc = heap.ArenaAllocator.init(heap.page_allocator);
    defer alloc.deinit();

    var args_iter = try process.argsWithAllocator(alloc.allocator());
    var args = std.ArrayList([]const u8).init(alloc.allocator());
    while (args_iter.next()) |arg| {
        try args.append(arg);
    }

    if (args.items.len != 2) {
        sdp("Expected: <arg>\n", .{});
        process.exit(1);
    }

    const filename = args.items[1];
    const cwd = fs.cwd();
    const openFlags = fs.File.OpenFlags{ .mode = fs.File.OpenMode.read_only };
    const file = try cwd.openFile(filename, openFlags);
    const contents = try file.reader().readAllAlloc(alloc.allocator(), math.maxInt(usize));

    const mat = try parseInput(contents, alloc.allocator());
    var cnt: usize = 0;
    var i: usize = 0;
    var j: usize = 0;
    while (i < mat.rcnt) {
        j = 0;
        while (j < mat.ccnt) {
            if (mat.isVisible(i, j))
                cnt += 1;
            j += 1;
        }
        i += 1;
    }
    sdp("Visible trees = {}\n", .{cnt});

    i = 1;
    j = 1;
    var mxm: i32 = -1;
    while (i < mat.rcnt - 1) {
        j = 1;
        while (j < mat.ccnt - 1) {
            const val = mat.scenicScore(i, j);
            // sdp("{}, {} => {}\n", .{ i, j, val });
            if (val > mxm) {
                mxm = val;
            }
            j += 1;
        }
        i += 1;
    }
    sdp("Maximum scenic score = {}\n", .{mxm});
}

fn parseInput(input: []const u8, alloc: mem.Allocator) !Matrix {
    var iter = mem.split(u8, input, "\n");
    var data = std.ArrayList(i32).init(alloc);
    var rcnt: usize = 0;
    var ccnt: usize = 0;

    while (iter.next()) |line| {
        if (line.len == 0)
            break;
        rcnt = line.len;
        for (line) |c| {
            try data.append(c - '0');
        }
        ccnt += 1;
    }

    return Matrix.init(try data.toOwnedSlice(), rcnt, ccnt);
}

const Matrix = struct {
    data: []const i32,
    rcnt: usize,
    ccnt: usize,

    fn init(data: []const i32, rcnt: usize, ccnt: usize) Matrix {
        assert(data.len == rcnt * ccnt);
        return Matrix{ .data = data, .rcnt = rcnt, .ccnt = ccnt };
    }

    fn get(self: *const Matrix, row: usize, col: usize) i32 {
        assert(row < self.rcnt);
        assert(col < self.ccnt);
        return self.data[row * self.ccnt + col];
    }

    fn isVisible(self: *const Matrix, row: usize, col: usize) bool {
        return self.isVisibleDirection(row, col, Direction.Top) or self.isVisibleDirection(row, col, Direction.Bottom) or self.isVisibleDirection(row, col, Direction.Left) or self.isVisibleDirection(row, col, Direction.Right);
    }

    fn isVisibleDirection(self: *const Matrix, row: usize, col: usize, edge: Direction) bool {
        assert(row < self.rcnt);
        assert(col < self.ccnt);

        const Params = struct {
            ii: usize,
            ji: usize,
            ilim: usize,
            jlim: usize,
            di: usize,
            dj: usize,
        };

        const curr = self.get(row, col);
        const params: Params = switch (edge) {
            Direction.Top => .{ .ii = 0, .ji = col, .ilim = row, .jlim = self.ccnt, .di = 1, .dj = 0 },
            Direction.Bottom => .{ .ii = row + 1, .ji = col, .ilim = self.rcnt, .jlim = self.ccnt, .di = 1, .dj = 0 },
            Direction.Left => .{ .ii = row, .ji = 0, .ilim = self.rcnt, .jlim = col, .di = 0, .dj = 1 },
            Direction.Right => .{ .ii = row, .ji = col + 1, .ilim = self.rcnt, .jlim = self.ccnt, .di = 0, .dj = 1 },
        };

        var i = params.ii;
        var j = params.ji;
        while (i < params.ilim and j < params.jlim) {
            if (curr <= self.get(i, j))
                return false;
            i += params.di;
            j += params.dj;
        }
        return true;
    }

    fn scenicScoreDirection(self: *const Matrix, row: usize, col: usize, dir: Direction) i32 {
        assert(row < self.rcnt);
        assert(col < self.ccnt);

        const Params = struct {
            ilim: isize,
            jlim: isize,
            di: isize,
            dj: isize,
        };

        const curr = self.get(row, col);
        const params: Params = switch (dir) {
            Direction.Top => .{ .ilim = -1, .jlim = @intCast(isize, self.ccnt), .di = -1, .dj = 0 },
            Direction.Bottom => .{ .ilim = @intCast(isize, self.rcnt), .jlim = @intCast(isize, self.ccnt), .di = 1, .dj = 0 },
            Direction.Left => .{ .ilim = @intCast(isize, self.rcnt), .jlim = -1, .di = 0, .dj = -1 },
            Direction.Right => .{ .ilim = @intCast(isize, self.rcnt), .jlim = @intCast(isize, self.ccnt), .di = 0, .dj = 1 },
        };

        var cnt: i32 = 0;
        var i = @intCast(isize, row);
        var j = @intCast(isize, col);
        while (i != params.ilim and j != params.jlim) {
            cnt += 1;
            if (curr <= self.get(@intCast(usize, i), @intCast(usize, j)))
                if (!(i == @intCast(isize, row) and j == @intCast(isize, col)))
                    break;
            i += params.di;
            j += params.dj;
        }
        // sdp("{}\n", .{cnt});
        return cnt - 1;
    }

    fn scenicScore(self: *const Matrix, row: usize, col: usize) i32 {
        return self.scenicScoreDirection(row, col, Direction.Top) * self.scenicScoreDirection(row, col, Direction.Bottom) * self.scenicScoreDirection(row, col, Direction.Left) * self.scenicScoreDirection(row, col, Direction.Right);
    }
};

const Direction = enum {
    Top,
    Bottom,
    Left,
    Right,
};
