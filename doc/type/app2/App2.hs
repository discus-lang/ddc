

app2 b f g x
 = case b of {
 	True	-> (f, f x);
	False	-> (g, g x);
 };
