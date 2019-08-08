class DateTime implements Comparable<DateTime> {
  bool operator ==(other) {
    if (!(other is DateTime)) return false;
    return (_value == other._value && isUtc == other.isUtc);
  }
}
