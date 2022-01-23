use util::*;

mod util;

const CORRECT_ANSWER: &str = "\
1
2
Fizz!
4
Buzz!
Fizz!
7
8
Fizz!
Buzz!
11
Fizz!
13
14
FizzBuzz!
16
17
Fizz!
19
Buzz!
Fizz!
22
23
Fizz!
Buzz!
26
Fizz!
28
29
FizzBuzz!
31
32
Fizz!
34
Buzz!
Fizz!
37
38
Fizz!
Buzz!
41
Fizz!
43
44
FizzBuzz!
46
47
Fizz!
49
Buzz!
Fizz!
52
53
Fizz!
Buzz!
56
Fizz!
58
59
FizzBuzz!
61
62
Fizz!
64
Buzz!
Fizz!
67
68
Fizz!
Buzz!
71
Fizz!
73
74
FizzBuzz!
76
77
Fizz!
79
Buzz!
Fizz!
82
83
Fizz!
Buzz!
86
Fizz!
88
89
FizzBuzz!
91
92
Fizz!
94
Buzz!
Fizz!
97
98
Fizz!
Buzz!
";

fn rust_fizzbuzz() -> String {
    let mut output = String::new();

    for i in 1..=100 {
        if i % 3 == 0 {
            output += "Fizz";
            if i % 5 == 0 {
                output += "Buzz";
            }
            output += "!";
        } else if i % 5 == 0 {
            output += "Buzz!";
        } else {
            output += &i.to_string();
        }
        output += "\n";
    }

    output
}

#[test]
fn rust() {
    assert_eq!(rust_fizzbuzz(), CORRECT_ANSWER);
}

#[test]
fn fizzbuzz_minimalist() {
    assert_eq!(
        run(
            Code(
                r#"
Modulus takes Number and Divisor
While Number is as high as Divisor
Put Number minus Divisor into Number
    (blank line ending While block)
Give back Number
    (blank line ending function declaration)
Limit is 100
Counter is 0
Fizz is 3
Buzz is 5
Until Counter is Limit
Build Counter up
If Modulus taking Counter, Fizz is 0 and Modulus taking Counter, Buzz is 0
Say "FizzBuzz!"
Continue
    (blank line ending 'If' Block)
If Modulus taking Counter and Fizz is 0
Say "Fizz!"
Continue
    (blank line ending 'If' Block)	
If Modulus taking Counter and Buzz is 0
Say "Buzz!"
Continue
    (blank line ending 'If' Block)
Say Counter (Note that the EOF terminates the `Until` block.)"#
            ),
            Input("")
        ),
        CORRECT_ANSWER
    );
}

#[test]
fn fizzbuzz() {
    assert_eq!(
        run(
            Code(
                r#"\
Midnight takes your heart and your soul
While your heart is as high as your soul
Put your heart without your soul into your heart

Give back your heart

Desire is a lovestruck ladykiller
My world is nothing 
Fire is ice
Hate is water
Until my world is Desire,
Build my world up
If Midnight taking my world, Fire is nothing and Midnight taking my world, Hate is nothing
Shout "FizzBuzz!"
Take it to the top

If Midnight taking my world, Fire is nothing
Shout "Fizz!"
Take it to the top

If Midnight taking my world, Hate is nothing
Say "Buzz!"
Take it to the top

Whisper my world
    "#
            ),
            Input("")
        ),
        CORRECT_ANSWER
    );
}

#[test]
fn fizzbuzz_using_stacks_and_cast() {
    assert_eq!(
        run(
            Code(
                r#"\
Eternity takes the pain.
The prize is silence
Until the pain is nothing,
Roll the pain into violence,
Cast violence into your liesz,
Let the prize be with your liesz.

Give back the prize

Midnight takes your heart and your soul
While your heart is as high as your soul
Put your heart without your soul into your heart

Give back your heart

My dreams are diamond nightmares
My suffering is the sea
My screams are hollow hatred
Your love is meaningless to me

Rock my dreams like a razorblade smile
Rock my dreams with your love, your love
Rock my screams like a switchblade missile
Rock my screams with your love, your love.

Cast my suffering into starlight
Let surrender be eternity taking my dreams
Let salvation be eternity taking my screams

Desire is a lovestruck ladykiller
My world is nothing
Fire is ice
Hate is water
Until my world is desire,
Build my world up
If midnight taking my world, fire is nothing and midnight taking my world, hate is nothing
Shout surrender with salvation with starlight
Take it to the top

If midnight taking my world, fire is nothing
Shout surrender with starlight
Take it to the top

If midnight taking my world, hate is nothing
Shout salvation with starlight
Take it to the top

Whisper my world
    "#
            ),
            Input("")
        ),
        CORRECT_ANSWER
    );
}
