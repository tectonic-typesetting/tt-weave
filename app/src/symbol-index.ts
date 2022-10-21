import { ModuleId } from "./base";

export interface SymbolIndexEntry {
  d: ModuleId[];
  r: ModuleId[];
}

// This is added to the toplevel globals by the toplevel script tag that loads
// ttw/ttw-symbol-index.js. We should probably move this to a more
// component-y implementation.
declare global {
  var ttWeaveSymbolIndex: { [symbol: string]: SymbolIndexEntry } | undefined;
}

/// Returns `undefined` if either the index hasn't been loaded yet, or the
/// symbol isn't in it.
export function getSymbolIndexEntry(symbol: string): SymbolIndexEntry | undefined {
  var rec = undefined;

  if (globalThis.ttWeaveSymbolIndex !== undefined) {
    rec = globalThis.ttWeaveSymbolIndex[symbol];
  }

  return rec;
}

/// Returns an empty list if the index hasn't been loaded yet.
export function getSymbols(): string[] {
  // Use a callback or something rather than just allocating a big array? It
  // probably doesn't matter.
  if (globalThis.ttWeaveSymbolIndex === undefined) {
    return [];
  }

  // Looks like this should preserve order for us. See
  // https://stackoverflow.com/questions/5525795/
  return Object.keys(globalThis.ttWeaveSymbolIndex);
}
