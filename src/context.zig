const scanning = @import("scanning.zig");
const errors = @import("errors.zig");

const TokenIterator = scanning.TokenIterator;
const TokenContext = scanning.TokenContext;
const ErrorLog = errors.ErrorLog;

pub const Error = errors.Error;
pub const Warning = errors.Warning;

pub const Context = struct {
    tokenIterator: *TokenIterator,
    log: *ErrorLog,

    pub fn pushError(self: Context, err: Error) void {
        self.log.push(err, self.tokenIterator.peek(self.log));
    }
    pub fn pushWarning(self: Context, warn: Warning) void {
        self.log.pushWarn(warn, self.tokenIterator.peek(self.log));
    }
};
