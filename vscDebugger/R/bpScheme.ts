

type rFunction = any;

interface srcBreakpoint {
    file: string;
    breakpoints: breakpoint[];
    includePackages: boolean;
}

interface breakpoint {
    requestedLine?: number;
    line?: number; //ignore if verified==false
    maxOffset?: number;
    id?: number;
    attempted: boolean; //default false
    verified: boolean; //default false
    message?: string;
    rFunction?: rFunction; //only in R: function that contains the bp
    rAt?: number[][]; //only in R: step that contains the bp
}

// type mergeBreakpointList


// type mergeSrcBreakpointList = (bps: srcBreakpoint[]) => srcBreakpoint[]


declare function mergeSrcBreakpoints(bps: srcBreakpoint[]): srcBreakpoint[];
// declare function mergeSrcBreakpoints(bp1: srcBreakpoint, bp2: srcBreakpoint): srcBreakpoint;
declare function addBreakpoint(bp0: srcBreakpoint, bp1: breakpoint): srcBreakpoint;
declare function getBreakpointsForFunction(fnc: rFunction): breakpoint[];

declare function storeBreakpoint(bp: srcBreakpoint): void;



