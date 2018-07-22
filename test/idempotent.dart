class DartFormatter {
  DartFormatter(
      {this.lineEnding, int pageWidth, int indent, Iterable<StyleFix> fixes})
      : pageWidth = pageWidth ?? 80,
        indent = indent ?? 0 {}

  String format(String source, {uri}) {
    return formatSource(
            new SourceCode(source, uri: uri, isCompilationUnit: true))
        .text;
  }
}
