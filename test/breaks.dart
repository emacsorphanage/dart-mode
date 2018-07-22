class _LoginPageState extends State<LoginPage> {
  Widget build(BuildContext context) {
    return new Scaffold(
      body: new SafeArea(
        child: new ListView(
          padding: const EdgeInsets.symmetric(horizontal: 24.0),
          children: <Widget>[
            new Column(
            ),
            const SizedBox(height: 120.0),
            TextField(
              decoration: InputDecoration(
                filled: true,
              ),
            ),
    ])));
  }
}