import { ModuleId } from "./base";

export interface NamedModuleIndexEntry {
  n: string;
  d: ModuleId[];
  r: ModuleId[];
}

// This is added to the toplevel globals by the toplevel script tag that loads
// ttw/ttw-named-module-index.js. We should probably move this to a more
// component-y implementation.
declare global {
  var ttWeaveNamedModuleIndex: { [id: ModuleId]: NamedModuleIndexEntry } | undefined;
}

/// Returns `undefined` if either the index hasn't been loaded yet, or the
/// module isn't in it. The latter could happen if the module ID is invalid or
/// if it's not a named module.
export function getNamedModuleIndexEntry(mid: ModuleId): NamedModuleIndexEntry | undefined {
  var rec = undefined;

  if (globalThis.ttWeaveNamedModuleIndex !== undefined) {
    rec = globalThis.ttWeaveNamedModuleIndex[mid];
  }

  return rec;
}
