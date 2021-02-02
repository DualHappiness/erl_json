#[macro_use]
extern crate rustler;
// #[macro_use] extern crate lazy_static;

use rustler::{Decoder, Encoder, Env, Term};

mod atoms {
    rustler_atoms! {
        atom ok;
        atom undefined;
        atom error;
    }
}

rustler::rustler_export_nifs! {
    "erl_json",
    [
        ("decode", 1, decode),
        ("encode", 1, encode),
    ],
    None
}

#[derive(Debug)]
struct EncodeWrapper<'a>(&'a serde_json::Value);

impl<'a> Encoder for EncodeWrapper<'a> {
    fn encode<'b>(&self, env: rustler::Env<'b>) -> rustler::Term<'b> {
        use serde_json::Value;
        match &self.0 {
            Value::Null => atoms::undefined().encode(env),
            Value::Bool(bool) => bool.encode(env),
            Value::Array(array) => array
                .into_iter()
                .map(|v| EncodeWrapper(v).encode(env))
                .collect::<Vec<rustler::Term>>()
                .encode(env),
            Value::Number(n) => n.as_f64().unwrap().encode(env),
            Value::String(string) => string.encode(env),
            Value::Object(map) => {
                let map_term = rustler::types::map::map_new(env);
                match map.into_iter().try_fold(map_term, |map_term, (k, v)| {
                    map_term.map_put(k.encode(env), EncodeWrapper(v).encode(env))
                }) {
                    Ok(term) => term,
                    _ => atoms::error().encode(env),
                }
            }
        }
    }
}

#[derive(Debug)]
struct DecodeWrapper(serde_json::Value);

impl<'a> Decoder<'a> for DecodeWrapper {
    fn decode(term: rustler::Term<'a>) -> Result<Self, rustler::Error> {
        use rustler::TermType;
        use serde_json::Value;
        // ! 没写的都是不该出现的
        match term.get_type() {
            TermType::Number => {
                // ! 数字严格限定类型 一定是f64
                let n = serde_json::Number::from_f64(term.decode()?).unwrap();
                Ok(DecodeWrapper(Value::Number(n)))
            }
            TermType::EmptyList | TermType::List => {
                let mut array = Vec::new();
                for cell in term.decode::<rustler::types::ListIterator>()? {
                    let DecodeWrapper(v) = cell.decode()?;
                    array.push(v);
                }
                Ok(DecodeWrapper(Value::Array(array)))
            }
            TermType::Atom => match term.decode::<bool>() {
                Ok(b) => Ok(DecodeWrapper(Value::Bool(b))),
                _ => {
                    if term.as_c_arg() == atoms::undefined().as_c_arg() {
                        Ok(DecodeWrapper(Value::Null))
                    } else {
                        Err(rustler::Error::BadArg)
                    }
                }
            },
            TermType::Binary => {
                let chars: &[u8] = term.decode::<rustler::types::Binary>()?.as_slice();
                match String::from_utf8(chars.to_vec()) {
                    Ok(string) => Ok(DecodeWrapper(Value::String(string))),
                    Err(_) => Err(rustler::Error::BadArg),
                }
            }
            TermType::Map => {
                let mut map = serde_json::Map::new();
                for (k, v) in term.decode::<rustler::types::MapIterator>()? {
                    let key = k.decode()?;
                    let DecodeWrapper(value) = v.decode()?;
                    map.insert(key, value);
                }
                Ok(DecodeWrapper(Value::Object(map)))
            }
            _ => Err(rustler::Error::BadArg),
        }
    }
}

fn decode<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, rustler::Error> {
    let chars: &[u8] = args[0].decode::<rustler::types::Binary>()?.as_slice();
    let deserialized: serde_json::Value = serde_json::from_slice(chars).unwrap();
    Ok((atoms::ok(), EncodeWrapper(&deserialized)).encode(env))
}

fn encode<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, rustler::Error> {
    let value_wrapper: DecodeWrapper = args[0].decode()?;

    match serde_json::to_string(&value_wrapper.0) {
        Ok(string) => Ok((atoms::ok(), string).encode(env)),
        Err(_) => Err(rustler::Error::BadArg),
    }
}
