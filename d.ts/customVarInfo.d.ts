
import { RValue, REnvironment, RFunction, RCall, RNULL, RList, RVector } from './RTypes';
import { StackTree } from './buildStack';

type MinimalVariable = StackTree.MinimalVariable;
    
type ChildVarFunction = (rValue: RValue) => MinimalVariable[];

interface varInfo {
  // Human friendly name of the entry, informative purpose only
  name: string;
  // Function that determines if the entry is to be used for a given variable
  doesApply: ((v: RValue) => boolean);
  // The child variables (typically entries of a list etc.)
  childVars?: ChildVarFunction | MinimalVariable[];
  // Informative attributes. Meant to be added by the user. Names should be preceded by '__'
  customAttributes?: ChildVarFunction | MinimalVariable[];
  // Normal attributes. Can be overwritten internally to handle custom variable info (e.g. promises)
  internalAttributes?: ChildVarFunction | MinimalVariable[];
  // Can be used to check for children (attributes or childVars), without actually evaluating them, to improve performance
  hasChildren?: ((v: RValue) => boolean) | boolean;
  // String representation of the variable. Must be a single atomic string!
  toString?: ((v: RValue) => string) | string;
  // Type of the variable shown in the debugger
  type?: ((v: RValue) => string) | string;
  // Whether to include the normal attributes. Can be used to properly show custom variable info (e.g. promises)
  includeAttributes?: ((v: RValue) => boolean) | boolean;
  // Expression that can be evaluated to get the variable value. Used to copy variable as expression
  evaluateName?: ((v: RValue) => string) | string;
}

