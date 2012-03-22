import std.stdio;

uint gcd(uint x, uint y) {
	if (0 == y) return x;
	else return gcd(y, x % y);
}

struct Rational {
	this(uint _num, uint _denum) {
		num = _num; denum = _denum;
		normalize();
	}

	void opMulAssign(const ref Rational rhs) {
		num *= rhs.num; denum *= rhs.denum;
		normalize();
	}
	
	void normalize() {
		auto _gcd = gcd(num, denum);
		while (_gcd != 1) {
			num /= _gcd;
			denum /= _gcd;
			_gcd = gcd(num, denum);
		}
	}

 	uint num, denum;
}

bool check(uint x, uint y) {
	if (x / 10 == y % 10)
		return Rational(x, y) == Rational(x % 10, y / 10);
	if (x % 10 == y / 10)
		return Rational(x, y) == Rational(x / 10, y % 10);
	return false;
}

void main() {
	Rational[] rationals;
 	foreach (x; 10 .. 100)
		foreach (y; x + 1 .. 100)
			if (check(x, y)) {
				rationals ~= Rational(x, y);
				writeln(x, "/", y);
			}
	auto result = Rational(1, 1);
	foreach (r; rationals) {
		writeln(r.num, "/", r.denum);
		result *= r;
	}
	writeln(result.denum);
}
