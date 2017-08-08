package fun

import (
	"encoding/json"
)

type (
	_ll List
	_id Ident
	_kw Keyword
	_op Operator
	_tp Type
	_sl String
	_cl Char
	_il Integer
	_dl Double
	_bl Bool
)

// MarshalJSON is a custom marshaler.
func (x List) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_ll
	}{
		_ll:  _ll(x),
		Type: "List",
	})
}

// MarshalJSON is a custom marshaler.
func (x Ident) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_id
	}{
		_id:  _id(x),
		Type: "Ident",
	})
}

// MarshalJSON is a custom marshaler.
func (x Keyword) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_kw
	}{
		_kw:  _kw(x),
		Type: "Keyword",
	})
}

// MarshalJSON is a custom marshaler.
func (x Operator) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_op
	}{
		_op:  _op(x),
		Type: "Operator",
	})
}

// MarshalJSON is a custom marshaler.
func (x Type) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_tp
	}{
		_tp:  _tp(x),
		Type: "Type",
	})
}

// MarshalJSON is a custom marshaler.
func (x String) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_sl
	}{
		_sl:  _sl(x),
		Type: "String",
	})
}

// MarshalJSON is a custom marshaler.
func (x Char) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_cl
	}{
		_cl:  _cl(x),
		Type: "Char",
	})
}

// MarshalJSON is a custom marshaler.
func (x Integer) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_il
	}{
		_il:  _il(x),
		Type: "Integer",
	})
}

// MarshalJSON is a custom marshaler.
func (x Double) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_dl
	}{
		_dl:  _dl(x),
		Type: "Double",
	})
}

// MarshalJSON is a custom marshaler.
func (x Bool) MarshalJSON() ([]byte, error) {
	return json.Marshal(struct {
		Type string `json:"type"`
		_bl
	}{
		_bl:  _bl(x),
		Type: "Bool",
	})
}
