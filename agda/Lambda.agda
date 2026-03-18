module Lambda where

open import Data.List
open import Agda.Primitive renaming (Set to Type ; Setω to Typeω)
open import Data.Unit

pattern _▸_ as a = a ∷ as
pattern ∅ = []

infix 4 _∈_

data _∈_ {A : Type} : (a : A) → List A -> Type where
  Z : ∀ {Γ : List A}{n : A} -> n ∈ Γ ▸ n

  S : ∀ {Γ n m}
    -> n ∈ Γ
    -> n ∈ Γ ▸ m

Ctxt : Set
Ctxt = List ⊤

data Tm : Ctxt → Type where
  var : ∀ {Γ v} → v ∈ Γ → Tm Γ
  _∙_ : ∀ {Γ} → Tm Γ → Tm Γ → Tm Γ
  fun : ∀ {Γ v} → Tm (Γ ▸ v) → Tm Γ

indTm : ∀ (P : (Γ : Ctxt) → Tm Γ → Type) →
  (∀ {Γ v} → (idx : v ∈ Γ) → P Γ (var idx)) →
  (∀ {Γ fn arg} → P Γ fn → P Γ arg → P Γ (fn ∙ arg)) →
  (∀ {Γ v body} → P (Γ ▸ v) body → P Γ (fun body)) →
  ∀ {Γ} → (tm : Tm Γ) → P Γ tm
indTm P onVar onApp onLam (var v) = onVar v
indTm P onVar onApp onLam (fn ∙ arg) =
  onApp (indTm P onVar onApp onLam fn) (indTm P onVar onApp onLam arg)
indTm P onVar onApp onLam (fun body) =
  onLam (indTm P onVar onApp onLam body)
