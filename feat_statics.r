DERVISH_AC = function(build, options, feat_list){ 
  if(build$weapon$hands == "Weapon-and-Shield") return(0)
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = (options$currentArmorType %in% c("None", "Light"))
  if(armorPass) return(1)
  return(0)
}

SPELLCASTING_DAMAGE = function(build, options, feat_list, class_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  #spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(cbind(names(spell_list), spell_list))]
  caster_level = g_casterLevel(build, options, class_n, feat_list, build$caster_progression[[class_n]])
  spell_stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  damage = 0
  if(length(uniq_spell_list) == 0) return(damage)
  for(ind in 1:length(uniq_spell_list)){
    spell_n = uniq_spell_list[ind]
    metamagic = names(spell_n)
    if(is.null(metamagic)) metamagic = "None"
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$damage)) next
    
    number_of_times_cast = sum(spell_list == spell_n & names(spell_list) == metamagic, na.rm=T)
    
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod) * number_of_times_cast
    DURATION_RATIO = min(time / (REST_TIMER* g_level(build)), 1)
    
    damage = damage + spells_av[[spell_n]]$damage(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod) * DURATION_RATIO
  }
  
  return(damage)
}

SPELLCASTING_ENHANCEMENT_BONUS = function(build, options, feat_list, class_n, stable_bonus){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  #spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(cbind(names(spell_list), spell_list))]
  caster_level = g_casterLevel(build, options, class_n, feat_list, build$caster_progression[[class_n]])
  spell_stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  enhancement_bonus = stable_bonus
  if(length(uniq_spell_list) == 0) return(enhancement_bonus)
  for(ind in 1:length(uniq_spell_list)){
    spell_n = uniq_spell_list[ind]
    metamagic = names(spell_n)
    if(is.null(metamagic)) metamagic = "None"
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$enhancement_bonus)) next
    
    number_of_times_cast = sum(spell_list == spell_n & names(spell_list) == metamagic, na.rm=T)
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod) * number_of_times_cast
    DURATION_RATIO = min(time / (REST_TIMER * g_level(build)), 1)
    
    spell_EB = spells_av[[spell_n]]$enhancement_bonus(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod)
    spell_EB = enhancement_bonus + (enhancement_bonus - spell_EB) * DURATION_RATIO
    
    enhancement_bonus = pmax(enhancement_bonus, spell_EB)
  }
  
  return(round(enhancement_bonus))
}

sfd = 0
SPELLCASTING_FIXED_DAMAGE = function(build, options, feat_list, class_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  sfd <<- sfd + 1
  
  spell_list = g_spells(build, options, class_n, feat_list)
  #spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(cbind(names(spell_list), spell_list))]
  caster_level = g_casterLevel(build, options, class_n, feat_list, build$caster_progression[[class_n]])
  spell_stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  fixed_damage = c()
  if(length(uniq_spell_list) == 0) return(0)
  for(ind in 1:length(uniq_spell_list)){
    spell_n = uniq_spell_list[ind]
    metamagic = names(spell_n)
    if(is.null(metamagic)) metamagic = "None"
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$fixed_damage)) next
    
    number_of_times_cast = sum(spell_list == spell_n & names(spell_list) == metamagic, na.rm=T)
    spell_damage = spells_av[[spell_n]]$fixed_damage(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod)
    fixed_damage = c(fixed_damage, rep(spell_damage, number_of_times_cast))
    
  }
  
  return(fixed_damage)
}

SPELLCASTING_MAX_DURATION_TRAIT = function(build, options, feat_list, class_n, trait_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  #spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(cbind(names(spell_list), spell_list))]
  caster_level = g_casterLevel(build, options, class_n, feat_list, build$caster_progression[[class_n]])
  spell_stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  trait = 0
  if(length(uniq_spell_list) == 0) return(trait)
  for(ind in 1:length(uniq_spell_list)){
    spell_n = uniq_spell_list[ind]
    metamagic = names(spell_n)
    if(is.null(metamagic)) metamagic = "None"
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]][[trait_n]])) next
    
    number_of_times_cast = sum(spell_list == spell_n & names(spell_list) == metamagic, na.rm=T)
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod) * number_of_times_cast
    DURATION_RATIO = min(time / (REST_TIMER * g_level(build)), 1)
    
    new_trait = spells_av[[spell_n]][[trait_n]](build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod)
    
    trait = max(trait, new_trait)
  }
  
  return(trait)
}

SPELLCASTING_CUMULATIVE_TRAIT = function(build, options, feat_list, class_n, trait_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  #spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(cbind(names(spell_list), spell_list))]
  caster_level = g_casterLevel(build, options, class_n, feat_list, build$caster_progression[[class_n]])
  spell_stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  trait = 0
  if(length(uniq_spell_list) == 0) return(trait)
  for(ind in 1:length(uniq_spell_list)){
    spell_n = uniq_spell_list[ind]
    metamagic = names(spell_n)
    if(is.null(metamagic)) metamagic = "None"
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]][[trait_n]])) next
    
    number_of_times_cast = sum(spell_list == spell_n & names(spell_list) == metamagic, na.rm=T)
    
    trait = trait + spells_av[[spell_n]][[trait_n]](build, options, feat_list, class_n, metamagic, caster_level, spell_stat_mod) * number_of_times_cast
  }
  
  return(trait)
}

SWASHBUCKLER_AC = function(build, options, feat_list){ 
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = (options$currentArmorType %in% c("None", "Light"))
  if(armorPass) return(1)
  return(0)
}

MONK_AC = function(build, options, feat_list){ 
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = options$currentArmorType %in% c("None")
  if(armorPass) return(1)
  return(0)
}

FAVORED_ENEMY_DAMAGE = function(build, options, feat_list){ 
  if(is.null(options$allow_favoredEnemy)) return(0)
  if(!options$allow_favoredEnemy) return(0)
  return(1)
}

SNEAK_ATTACK = function(build, options, feat_list){
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma")
  if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
  BLUFF_RATIO = max(min((bluff_skill - g_level(build) + 10) / 20, 0.95), 0.05)
  SNEAK_RATIO = 0.2
  if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
  SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
  return(g_aveDamageDice(1,6) * SA_RATIO)
}

TEMPEST_DEFENSE_FUNCTION = function(build, options, feat_list){ 
  # Do I have Two-Weapon Fighting Feat?
  if(is.null(feat_list)) feat_list = g_feats(build)
  featPass = hasAllFeats(build, "Two-Weapon Fighting", feat_list)
  
  # Do I wear Light or no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = options$currentArmorType %in% c("None", "Light")
  
  # Do I want to be Two-Weapon Fighting
  buildPass = FALSE
  if(!is.null(build$weapon$hands)) buildPass = (build$weapon$hands == "Two-Weapon")
  
  if(featPass 
     & armorPass
     & buildPass){ return(1) }
  return(0)
}

BLEEDING_WOUND = function(build, options, feat_list){
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  if(build$weapon$size != "Light") return(0)
  bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma")
  if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
  BLUFF_RATIO = max(min((bluff_skill - g_level(build) + 10) / 20, 0.95), 0.05)
  SNEAK_RATIO = 0.2
  if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
  SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
  return(6 * SA_RATIO)
}
DERVISH_DANCE = function(build, options, feat_list){ 
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  bonus = floor((g_classLevel(build, "Dervish")+1)/2)
  boost_ratio = min((bonus * g_skillMaxes(build, "perform") / 20) / (g_level(build) * FIGHTING_RATIO), 1)
  return(bonus * boost_ratio)
}
KAMA_PLUSONE = function(build, options, feat_list){ 
  if(build$weapon$name == "Kama") { return(1) } else { return(0) }
}
PLUSONE = function(build, options, feat_list){ return(1) }

TWO_WEAPON_FIGHTING_OFF_ATTACKS = function(build, options, feat_list){ 
  if(build$weapon$hands == "Two-Weapon") return(1)
  return(0)
}