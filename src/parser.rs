use nom::IResult;
use nom::bytes::complete::take;
use nom::number::complete::be_u16;

pub fn length_value(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let(input, length) = be_u16(i)?;
    take(length)(input)
}
