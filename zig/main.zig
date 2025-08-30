const std = @import("std");
const Memo = @import("memo.zig").Memo;

fn isPrime(n: u64) bool {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;

    const limit = std.math.sqrt(@as(f64, @floatFromInt(n)));
    var i: u64 = 3;
    while (@as(f64, @floatFromInt(i)) <= limit) : (i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

fn isPrimeList(ns: []const u64) bool {
    for (ns) |n| {
        if (!isPrime(n)) return false;
    }
    return true;
}

fn isPrimeListAdapter(args: []const u64) bool {
    return isPrimeList(args);
}

fn primes() []const u64 {
    return &[_]u64{
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
        31, 37, 41, 43, 47,
        53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
        101, 103, 107, 109, 113, 127, 131, 137, 139, 149,
        1_981_201_020_802_099,
        144_403_552_893_599,
        29_100_036_564_239,
        1_012_020_412_021,
        166_666_666_667,
        10_999_999_999,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const check = gpa.deinit();
        std.debug.assert(check == .ok); // assert no leaks
    }
    const allocator = gpa.allocator();

    var memo = try Memo.init(allocator, &isPrimeListAdapter);
    defer memo.deinit();

    const input = primes();

    var timer = try std.time.Timer.start();

    // First call (compute)
    timer.reset();
    const r1 = try memo.call(input);
    if (!r1) return error.UnexpectedCompositeFound;
    const t1_ns = timer.read();

    // Second call (memoized)
    timer.reset();
    _ = try memo.call(input); // discard result explicitly
    const t2_ns = timer.read();

    const first_ms: f64 = @as(f64, @floatFromInt(t1_ns)) / 1_000_000.0;
    const second_ms: f64 = @as(f64, @floatFromInt(t2_ns)) / 1_000_000.0;
    const speedup: f64 = if (second_ms > 0.0) first_ms / second_ms else std.math.inf(f64);

    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const out = bw.writer();

    if (std.math.isFinite(speedup)) {
        try out.print(
            "[zig:all-primes] firstMs={d:.3}ms secondMs={d:.3}ms speedup={d:.1}x\n",
            .{ first_ms, second_ms, speedup },
        );
    } else {
        try out.print(
            "[zig:all-primes] firstMs={d:.3}ms secondMs={d:.3}ms speedup=∞x\n",
            .{ first_ms, second_ms },
        );
    }
    try bw.flush();
}
