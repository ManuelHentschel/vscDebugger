
import { DebugProtocol } from './debugProtocol';

import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';

export declare module Breakpoints {

  interface InternalBreakpoint extends DebugProtocol.SourceBreakpoint, DebugProtocol.Breakpoint {
    /** The source line of the breakpoint or logpoint. */
    line: number;
    /** An optional source column of the breakpoint. */
    column?: number;

    id: number;

    maxOffset?: number; // the maximum difference between requested line and actual breakpoint

    rAt?: RVector<number>[]; // the step in the R parse tree that contains the bp
    rFunction?: RFunction;

    source?: DebugProtocol.Source // For compatibility with DebugProtocol.Breakpoint // Must be same as containing FileBreakpoints

    endLine?: number; // In case the breakpoint spans multiple lines
    endColumn?: number; // In case the breakpoint spans only part of a line

    verified: boolean; // Whether this breakpoint was successfully set
    attempted: boolean; // Whether it has been attempted to set this breakpoint
    requestedLine: number; // The line requested (might differ from actual line)
  }

  // Used to internally store breakpoints:
  interface FileBreakpoints extends DebugProtocol.SetBreakpointsArguments {
    source: DebugProtocol.Source; // must match bp.source for bp in breakpoints
    breakpoints: InternalBreakpoint[];
  }

  function getFileBreakpoints(path: string): FileBreakpoints;

  function getBreakpointLines(path: string): number[];

  function getRequestedBreakpointLines(path: string): number[];

}
