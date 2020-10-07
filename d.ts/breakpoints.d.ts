
import { DebugProtocol } from './debugProtocol';

import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';

export declare module Breakpoints {
  interface Source extends DebugProtocol.Source {
    path: string; // empty string "" when no valid path provided
  }

  interface InternalBreakpoint extends DebugProtocol.SourceBreakpoint, DebugProtocol.Breakpoint, DebugProtocol.BreakpointLocation {

    // id
    id: number;

    // location
    source: Source
    line: number;
    column?: number;
    endLine?: number;
    endColumn?: number

    // (optional) details
    condition?: string;
    hitCondition?: string;
    logMessage?: string;

    // status of the bp
    verified: boolean;
    message?: string; // An optional message about the state of the breakpoint

    // internals
    attempted: boolean; // Whether it has been attempted to set this breakpoint
    requestedLine: number; // The line requested (might differ from actual line)
    rFunction?: RFunction;
    maxOffset?: number; // the maximum difference between requested line and actual breakpoint
  }

  // Used to internally store breakpoints:
  interface SourceBreakpoints extends DebugProtocol.SetBreakpointsArguments {
    source: Source; // must match bp.source for bp in breakpoints
    breakpoints: InternalBreakpoint[];

    // // not used:
    // lines?: number[];
    // sourceModified?: boolean;
  }

  interface Reference {
    name: string;
    env: REnvironment;
    at: RVector<number>;
    filename: string;
    line: number;
    timediff: number;
  }

  function findLineNum(
    srcfile: string, 
    line: number, 
    nameonly: boolean, 
    envir: REnvironment, 
    lastenv: REnvironment
  ): Reference[];

  interface SummarizedReference {
    name: string;
    env: REnvironment;
    at: RList<RVector<number>>;
    line: RList<number>;
    timediff: RList<number>;
  }

  function summarizeRefs(refs: Reference[]): SummarizedReference[];

  function getSourceBreakpoints(path: string): SourceBreakpoints;

  function setBreakpointsRequest(
    response: DebugProtocol.SetBreakpointsResponse,
    args: DebugProtocol.SetBreakpointsArguments,
    request: DebugProtocol.SetBreakpointsRequest
  ): void;
}
