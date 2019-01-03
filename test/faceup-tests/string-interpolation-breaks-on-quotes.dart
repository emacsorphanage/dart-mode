class ArgumentError extends Error {
  String get _errorName => "Invalid argument${!_hasValue ? "(s)" : ""}";
}
