/*
JSON

escapes:
\", \\, \/,
\n, \r, \t,
\b, backspace, 0x08,
\f, form feed, 0x0C
\uXXXX

number: -?[0-9]+(.[0-9]+)?([eE][-+]?[0-9]+)?
*/

use std::collections::HashMap;
use std::num::ParseFloatError;

use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{char, digit1, multispace0, satisfy};
use nom::character::streaming::one_of;
use nom::combinator::{map, map_opt, map_res, opt, recognize, value};
use nom::error::{FromExternalError, ParseError, dbg_dmp};
use nom::lib::std::borrow::Cow;
use nom::multi::{fold_many_m_n, many0, many1};
use nom::sequence::{preceded, separated_pair, terminated, tuple};
use nom::Parser;
use nom::{AsChar, IResult};

fn parse_null<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, (), E> {
    value((), tag("null")).parse(input)
}

fn parse_bool<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, bool, E> {
    alt((value(true, tag("true")), value(false, tag("false")))).parse(input)
}

fn parse_number<
    'i,
    E: ParseError<&'i str> + FromExternalError<&'i str, std::num::ParseFloatError>,
>(
    input: &'i str,
) -> IResult<&'i str, f64, E> {
    map_res(alt((
        // Case .42
        recognize(
            tuple((
                char('.'),
                decimal,
                opt(tuple((
                    one_of("eE"),
                    opt(one_of("+-")),
                    decimal
                ))) 
            ))
        ),
        // Case 42e42, 42.42e42
        recognize(
            tuple((
                decimal,
                opt(preceded(
                    char('.'),
                    decimal,
                )),
                one_of("eE"),
                opt(one_of("+=")),
                decimal
            ))
        ),
        // Case 42. 42.42
        recognize(
            tuple((
                decimal,
                char('.'),
                opt(decimal)
            ))
        )
    )),
    |float_str: &'i str| float_str.parse(),
    ).parse(input)
}

fn decimal<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, &'i str, E> {
    recognize(
        many1(
            terminated(one_of("0123456789"), many0(char('_')))
        )
    ).parse(input)
}

enum StrFragment<'a> {
    Unescaped(&'a str),
    Escaped(char),
}

fn parse_escaped_unicode<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, char, E> {
    preceded(
        char('u'),
        map_opt(
            map(
                recognize(fold_many_m_n(
                    4,
                    4,
                    satisfy(|c| c.is_hex_digit()),
                    || (),
                    |(), _c| (),
                )),
                |hex_str| u32::from_str_radix(hex_str, 16).unwrap(),
            ),
            |code_point| std::char::from_u32(code_point),
        ),
    )
    .parse(input)
}
/*
escapes:
\", \\, \/,
\n, \r, \t,
\b, backspace, 0x08,
\f, form feed, 0x0C
\uXXXX
*/

fn parse_escaped_char<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, char, E> {
    preceded(
        char('\\'),
        alt((
            value('"', char('"')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            parse_escaped_unicode,
        )),
    )
    .parse(input)
}

fn parse_str_fragment<'i, E: ParseError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, StrFragment<'i>, E> {
    alt((
        map(parse_escaped_char, StrFragment::Escaped),
        map(is_not("\"\\"), StrFragment::Unescaped),
    ))
    .parse(input)
}

fn parse_str<'i, E: ParseError<&'i str>>(input: &'i str) -> IResult<&'i str, Cow<'i, str>, E> {
    let mut result = Cow::Borrowed("");

    let mut parse_double_quotes = char('"');
    let (mut input, _) = parse_double_quotes.parse(input)?;

    loop {
        let dq_err = match parse_double_quotes.parse(input) {
            Ok((tail, _)) => return Ok((tail, result)),
            Err(nom::Err::Error(err)) => err,
            Err(err) => return Err(err),
        };

        let tail = match parse_str_fragment(input) {
            Ok((tail, StrFragment::Escaped(c))) => {
                result.to_mut().push(c);
                tail
            }
            Ok((tail, StrFragment::Unescaped(s))) => {
                if result.is_empty() {
                    result = Cow::Borrowed(s);
                } else {
                    result.to_mut().push_str(s);
                }
                tail
            }
            Err(nom::Err::Error(err)) => return Err(nom::Err::Error(dq_err.or(err))),
            Err(err) => return Err(err),
        };
        input = tail
    }
}

fn parse_comma_seperated_json_thing<'i, T, E: ParseError<&'i str>, C>(
    open_delimiter: char,
    close_delimiter: char,
    mut subparser: impl Parser<&'i str, T, E>,
    empty_collection: impl Fn() -> C,
    collection_fold: impl Fn(C, T) -> C,
) -> impl Parser<&'i str, C, E> {
    let mut parse_open = tuple((char(open_delimiter), multispace0));
    let mut parse_close = tuple((multispace0, char(close_delimiter)));
    let mut parse_comma = tuple((multispace0, char(','), multispace0));

    move |input: &'i str| {
        let (mut input, _) = parse_open.parse(input)?;
        let mut collection = empty_collection();

        match parse_close.parse(input) {
            Ok((tail, _)) => return Ok((tail, collection)),
            Err(nom::Err::Error(_err)) => {}
            Err(err) => return Err(err),
        };

        loop {
            let (tail, item) = subparser.parse(input)?;

            collection = collection_fold(collection, item);
            input = tail;

            let err1 = match parse_close.parse(input) {
                Ok((tail, _)) => return Ok((tail, collection)),
                Err(nom::Err::Error(err)) => err,
                Err(err) => return Err(err),
            };

            match parse_comma.parse(input) {
                Ok((tail, _)) => input = tail,
                Err(nom::Err::Error(err2)) => return Err(nom::Err::Error(err1.or(err2))),
                Err(err) => return Err(err),
            }
        }
    }
}
#[derive(Debug,Clone)]
    enum JSONValue<'i> {
    Null,
    Bool(bool),
    Number(f64),
    Str(Cow<'i, str>),
    Array(Vec<JSONValue<'i>>),
    Object(HashMap<Cow<'i, str>, JSONValue<'i>>),
}

fn parse_array<'i, E: ParseError<&'i str>+ FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, Vec<JSONValue<'i>>, E> {
    parse_comma_seperated_json_thing('[', ']', parse_value, Vec::new, |mut vec, item| {
        vec.push(item);
        vec
    })
    .parse(input)
}

fn parse_object<'i, E: ParseError<&'i str>+ FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, HashMap<Cow<'i, str>, JSONValue<'i>>, E> {
    parse_comma_seperated_json_thing(
        '{',
        '}',
        separated_pair(
            parse_str,
            tuple((multispace0, char(':'), multispace0)),
            parse_value,
        ),
        HashMap::new,
        |mut map, (key, value)| {
            map.insert(key, value);
            map
        },
    )
    .parse(input)
}

fn parse_value<'i, E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>>(
    input: &'i str,
) -> IResult<&'i str, JSONValue<'i>, E> {
    alt((
        value(JSONValue::Null, parse_null),
        map(parse_bool, JSONValue::Bool),
        map(parse_number, JSONValue::Number),
        map(parse_str, JSONValue::Str),
        map(parse_array, JSONValue::Array),
        map(parse_object, JSONValue::Object),
    ))
    .parse(input)
}

fn main() {
    let input = r#"{"a": 10.}"#;
    
    let (_tail, value) = parse_value::<()>(input).unwrap();
    
    println!("{:#?}", value)
}
