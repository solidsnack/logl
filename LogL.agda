
module LogL where

open import Data.List using (List)
open import Data.String
open import Data.Nat
open import Data.Unit


data UUID : Set where
  uuid : UUID

data Log : Set where
  log : UUID → Log

data Map : Set → Set → Set where
  m : Map ⊤ ⊤    -- Haha.

data Time : Set where
  time : String → Time

data MessageID : Set where
  messageID : UUID → MessageID

data Message : Set where
  message : String → Message


data Interval (T : Set) : Set where 
  ⟨⋯⟩   : Interval T
  [_]   : T → Interval T
  ⟨_⋯   : T → Interval T
  ⋯_⟩   : T → Interval T
  ⟨_⋯_⟩ : T → T → Interval T
  [_⋯   : T → Interval T
  ⋯_]   : T → Interval T
  [_⋯_] : T → T → Interval T
  ⟨_⋯_] : T → T → Interval T
  [_⋯_⟩ : T → T → Interval T



data String/Q : Set where
  ^_    : String → String/Q
  _$    : String → String/Q
  *_*   : String → String/Q

data Q : Set where
  str/Q : String/Q → Q
  time  : Interval Time → Q
  str   : Interval String → Q
  _∧_   : Q → Q → Q
  _∨_   : Q → Q → Q
  ¬_    : Q → Q

data Rewrite/L : Set where
  ⟨⟩  : Rewrite/L
  _←_ : MessageID → Message → Rewrite/L
  _!  : MessageID → Rewrite/L
  _&_ : Rewrite/L → Rewrite/L → Rewrite/L 

data Append/L : Set → Set where
  append   : Log → Message → Append/L MessageID
  search   : Log → Q → Append/L (List MessageID)
  retrieve : Log → List MessageID → Append/L (Map MessageID Message)
  new      : Append/L Log
  update   : Log → Rewrite/L → Append/L Log
  delete   : Log → Append/L ⊤
  --  Program composition, what will it mean?
  --  For now, it just means tupled execution. Tuples of Append/L t
  --
  --    (e₁ : Append/L MessageID, e₂ : Append/L Log, ...)
  --
  --  can be evaluated in bulk but separately from one another. There is
  --  no variable binding.
  --
