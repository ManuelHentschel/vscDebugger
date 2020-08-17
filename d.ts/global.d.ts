

import { Breakpoints } from './breakpointManagement';
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';

export declare module Session {
  interface Session {
    // settings:
    setBreakpointsInPackages: boolean;
    allowGlobalDebugging: boolean;
    breakOnErrorFromConsole: boolean;
    breakOnErrorFromFile: boolean;
    assignToAns: boolean;

    overwritePrint: boolean;
    overwriteCat: boolean;
    overwriteSource: boolean;

    rStrings: {
      delimiter0: string;
      delimiter1: string;
      prompt: string;
      continue: string;
      append: string;
    }

    threadId: number; //dummy, but must match the one used in the DAP host

    // server
    useJsonServer?: boolean;
    jsonPort?: number;
    jsonHost?: string;
    jsonServerConnection?: RValue; // only if useJsonServer==TRUE

    useSinkServer?: boolean;
    sinkPort?: number;
    sinkHost?: string;
    sinkServerConnection?: RValue; // only if useSinkServer==TRUE

    // debugSession:
    noDebug: boolean; // todo
    debugMode: ("function" | "file" | "workspace")
    workingDirectory: string;
    file: string;
    mainFunction: string;
    includePackageScopes: boolean;

    // state:
    isInitialized: boolean;
    isConfigurationDone: boolean;
    isEvaluating: boolean;
    isError: boolean;
    entryFrames: number[];
    launchFrames: number;
    ignoreNextCallback: boolean;

    // data:
    breakpointId: number;
    fileBreakpoints: Breakpoints.FileBreakpoints[];
  }
}
