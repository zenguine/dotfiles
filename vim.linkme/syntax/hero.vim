" Vim syntax file
" Language:     Darkdynasty hero dsl files
" Maintainer:   Justin Cullen
" Last Change:  Jan 29, 2014
" Version:      1

if exists("b:current_syntax")
  finish
endif

setlocal iskeyword+=:
setlocal iskeyword+=_
syn case ignore

syn match heroOperator "\v\^"
syn match heroOperator "\v\&"
syn keyword defAbility define_ability nextgroup=abilityAttrs
syn keyword selectTarget select_target
syntax keyword actionType damage heal clear_buffs apply_buff remove_buff
syntax match targetSpec "\v\@named" contains=namedTarget nextgroup=namedTarget
syntax match namedTarget "\v\{\a*\}"
syntax match targetSpec "\v\@neighbors" 
syn region abilityAttrs start=/\[/ end=/\]/ contains=abilityAttr nextgroup=abilityDesc
syn keyword abilityAttr contained name type cooldown orb_cost 

highlight link heroOperator Operator
highlight link namedTarget Function
highlight link targetName String
highlight link abilityDesc String
highlight link defAbility Keyword
highlight link actionType Special
highlight link targetSpec Function
highlight link abilityAttr Keyword
highlight link abilityAttrs String
highlight link selectTarget Define


let b:current_syntax = "hero"
