

// These types have no meaning in typescript
// Can be used to specify types that are only used within R

export type RValue = any
export type RFunction = RValue;
export type RCall = RValue;
export type RNULL = undefined;
export type RInteger = number;

// the notation Type[] is equivalent to Array<Type> and to be interpreted as list() in R
// for named lists, use typescript objects
export type RList<T> = Array<T>; 

// to indicate vectors in R explicitly use RVector<T>
export type RVector<T extends number|string|boolean|RInteger> = Array<T>; 

// environments are a class, since they are used as base class for e.g. R6 classes
export class REnvironment {
  rEnvironment: RValue;
}

/*
The following typescript types are used directly:
type boolean = boolean;
type string = string;
type number = number;
*/


