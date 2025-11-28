
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { StackTree } from './stackTree';
import { DebugProtocol } from './debugProtocol';

type MinimalVariable = StackTree.MinimalVariable;
    
type ValueOrFunction<T> = ((v: RValue) => T) | T;

export interface VarInfo {
  // Human friendly name of the entry, informative purpose only
  name: string;
  // Function that determines if the entry is to be used for a given variable
  doesApply: ((v: RValue) => boolean);
  // The child variables (typically entries of a list etc.)
  childVars?: ((rValue: RValue, ind?: number[]) => MinimalVariable[]) | MinimalVariable[];
  // Number of childVars. Must match the length of the list returned by childVars()
  nChildVars?: ValueOrFunction<number>;
  // Informative attributes. Meant to be added by the user. Names should be preceded by '__'
  customAttributes?: ValueOrFunction<MinimalVariable[]>;
  // Normal attributes. Can be overwritten internally to handle custom variable info (e.g. promises)
  internalAttributes?: ValueOrFunction<MinimalVariable[]>;
  // String representation of the variable. Must be a single atomic string!
  toString?: ValueOrFunction<string>;
  // Type of the variable shown in the debugger
  type?: ValueOrFunction<string>;
  // Expression that can be evaluated to get the variable value. Used to copy variable as expression
  evaluateName?: ValueOrFunction<string>;
  // function that can be used to print/show the variable in the debug console
  printFunc?: RFunction | boolean;
  // If variable has a srcref, positive integer to retrieve its location
  locationReference?: ValueOrFunction<number>;
}

export function _vsc_applyVarInfo(
  v: RValue,
  info?: (keyof VarInfo)[],
  stackingInfos?: (keyof VarInfo)[],
  verbose?: boolean,
  ind?: RVector<number>
)

// Format used internally to compute and store location references
// Matches corresponding fields in:
// - LocationsResponse.body
// - OutputEvent.body
export interface LocationInfo {
  source: DebugProtocol.Source;
  line: number;
  column?: number;
  endLine?: number;
  endColumn?: number;
}
