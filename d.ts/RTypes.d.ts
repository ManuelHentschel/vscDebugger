

// These types have no meaning typescript
// Can be used to specify types that are only used within R

export type RValue = any
export type REnvironment = RValue;
export type RFunction = RValue;
export type RCall = RValue;
export type RNULL = undefined;

export type RList<T> = Array<T>; // the notation Type[] is equivalent to Array<Type> and to be interpreted as list() in R
export type RVector<T extends number|string|boolean> = Array<T>; // to indicate vectors in R explicitly use RVector<T>

/*
The following typescript types are used directly:
type boolean = boolean;
type string = string;
type number = number;
*/


