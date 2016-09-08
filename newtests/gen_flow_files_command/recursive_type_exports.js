// @flow

export type TRec1 = {a: TRec1};
export type TRec2 = {b: TRec3};
export type TRec3 = {c: TRec2};
export type TRec4 = {d: TRec5};
type TRec5 = {e: TRec1};

// tparams
export type TRec6<T> = {f: T, g: TRec5};
export type TRec7 = TRec6<number>;
//type TRec8<T> = {h: T, i: TRec8<T>};
//export type TRec9 = TRec8<string>;
