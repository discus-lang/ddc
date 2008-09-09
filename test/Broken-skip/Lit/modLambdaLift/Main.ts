
import Prelude;

f x
{
	g y	= x * x + y;
	g 3 + g 4;
}


traumaMain	
	= print $ (showInt $ f 6) % "\n";
