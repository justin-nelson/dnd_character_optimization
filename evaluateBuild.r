evaluateBuild = function(build, options=list(), level=NULL, levels_to_evaluate=NULL, base_build=0){
  offensiveMelee_info = evaluateBuild_offensiveMelee(build, options, level, levels_to_evaluate)
  
  score = offensiveMelee_info$score
  #if(score < base_build) score = base_build
  return(list(damage = offensiveMelee_info$damage,
              score = score - base_build))
}

evaluateBuild_offensiveMelee = function(build, options=list(), level=NULL, levels_to_evaluate=NULL){
  global_evaluations <<- global_evaluations + 1
  
  ####
  # Here is a bunch of code to make sure that levels and levels_to_evaluate are feasible
  ####
  if(is.null(level)) level = calculateLevel(build)
  if(is.null(levels_to_evaluate)) levels_to_evaluate = calculateLevel(build)
  if(level < 1) level = 1
  if(levels_to_evaluate < 1) levels_to_evaluate = 1
  if(levels_to_evaluate > level) levels_to_evaluate = level
  
  min_lv_to_eval = level - levels_to_evaluate + 1
  
  damage_per_level = rep(0, level)
  multiclass_penalty = rep(1, level)
  
  level_build = createPrefixBuild(build, calculateLevel(build) - level, F)
  
  for(i in min_lv_to_eval:level){
    cur_level_build = createPrefixBuild(level_build, calculateLevel(level_build) - i, F)
    damage_rolls = calculateDamage(cur_level_build, options)
    attack_rolls = calculateAttacks(cur_level_build, options)
    
    defense = i + 10
    
    damage_dealt       = calculateAverageDamage(attack_rolls, damage_rolls, defense)
    damage_per_level[i] = damage_dealt
    
    if(i < level){
      multiclass_penalty[i] = multiclassPenalty(cur_level_build) 
    }
  }
  
  mc_pen_multiplier = sum(min_lv_to_eval:level * multiclass_penalty[min_lv_to_eval:level]) / sum(min_lv_to_eval:level)
  
  score = sum(min_lv_to_eval:level * damage_per_level[min_lv_to_eval:level], na.rm=T) / sum(min_lv_to_eval:level) * mc_pen_multiplier
  
  num_feats = sum(unique(g_feats(build)) %in% g_featsInFeatType("Regular"))
  if(!is.null(options$feat_multiplier)){
    multiplier = 1+(num_feats * options$feat_multiplier)
    score = score * multiplier
  }
  if(!is.null(options$feat_adjustment)){
    adjustment = num_feats * options$feat_adjustment
    score = score + adjustment
  }
  
  return(list(damage = damage_per_level,
              score = score))
}

calculateDamage = function(build, options){
  damage = 0
  crit_chance = 1
  crit_chance_multiplier = 1
  crit_multiplier = 2
  postCrit_damage = 0
  onCrit_damage = 0
  
  feat_list = unique(g_feats(build))
  
  damage_multiplier = 1
  if(build$weapon$hands=="Two-Handed") damage_multiplier = 1.5
  
  
  damage = damage + build$weapon$damage
  damage = damage + build$weapon$enhancement_bonus # Enhancement Bonus
  damage = damage + (g_stat_mod(build, "Strength", feat_list) * damage_multiplier)
  
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["damage"]])){
      damage = damage + feats_av[[feat_n]][["damage"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["postCrit_damage"]])){
      postCrit_damage = postCrit_damage + feats_av[[feat_n]][["postCrit_damage"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["onCrit_damage"]])){
      onCrit_damage = onCrit_damage + feats_av[[feat_n]][["onCrit_damage"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["crit_chance"]])){
      crit_chance = crit_chance + feats_av[[feat_n]][["crit_chance"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["crit_chance_multiplier"]])){
      crit_chance_multiplier = crit_chance_multiplier * feats_av[[feat_n]][["crit_chance_multiplier"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["crit_multiplier"]])){
      crit_multiplier = crit_multiplier + feats_av[[feat_n]][["crit_multiplier"]](build, options, feat_list)
    }
  }
  
  crit_damage = (damage * crit_chance * crit_chance_multiplier * crit_multiplier) / 20
  noncrit_damage = (damage * (20-crit_chance)) / 20
  total_damage = crit_damage + noncrit_damage + postCrit_damage + (onCrit_damage * crit_chance * crit_chance_multiplier) / 20
  
  return(total_damage)
}

calculateAttacks = function(build, options){
  BAB = calculateBAB(build)
  attack_bonus = 0
  
  main_hand_attacks = c()
  off_hand_attacks = c()
  
  num_attacks_main = max(floor((BAB-1)/5)+1, 1)
  num_attacks_off = 0
  
  feat_list = unique(g_feats(build))
  
  attack_bonus = attack_bonus + max(build$weapon$enhancement_bonus, build$weapon$attack_bonus) #Enhancement Bonus
  attack_bonus = attack_bonus + g_stat_mod(build, "Strength", feat_list)
  
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["attack_bonus"]])){
      attack_bonus = attack_bonus + feats_av[[feat_n]][["attack_bonus"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["num_attacks_main"]])){
      num_attacks_main = num_attacks_main + feats_av[[feat_n]][["num_attacks_main"]](build, options, feat_list)
    }
    if(!is.null(feats_av[[feat_n]][["num_attacks_off"]])){
      num_attacks_off = num_attacks_off + feats_av[[feat_n]][["num_attacks_off"]](build, options, feat_list)
    }
  }
  
  # Special Code for Perfect Two Weapon Fighting
  if(hasAllFeats(build, "Perfect Two-Weapon Fighting") & build$weapon$hands=="Two-Weapon"){
    num_attacks_off = num_attacks_main
  }
  
  # Special Code for Flurry of Blows
  monk_level = g_classLevel(build, "Monk")
  if(monk_level <= 4) attack_bonus = attack_bonus - 1
  if(monk_level <= 8) attack_bonus = attack_bonus - 1
  if(hasAllFeats(build, "Flurry of Blows", feat_list) & build$weapon$name %in% MONK_WEAPON_LIST) {
    main_hand_attacks = c(main_hand_attacks, BAB+attack_bonus)
    num_attacks_off = num_attacks_off + 1
  }
  if(hasAllFeats(build, "Greater Flurry", feat_list) & build$weapon$name %in% MONK_WEAPON_LIST) {
    main_hand_attacks = c(main_hand_attacks, BAB+attack_bonus)
    num_attacks_off = num_attacks_off + 1
  }
  
  # Special Code for (not) two weapon fighting
  if(!hasAllFeats(build, "Two-Weapon Fighting", feat_list)){
    num_attacks_off = 0
  }
  
  top_attack_bonus = BAB + attack_bonus
  
  main_hand_attacks = c(main_hand_attacks, seq(from=top_attack_bonus, to=top_attack_bonus-5*(num_attacks_main-1), by=-5))
  if(num_attacks_off > 0) off_hand_attacks = c(off_hand_attacks, seq(from=top_attack_bonus, to=top_attack_bonus-5*(num_attacks_off-1), by=-5))
  
  attacks = c(main_hand_attacks, off_hand_attacks)
  
  return(attacks)
}

calculateScore = function(damage_per_level){ return(sum(1:30 * damage_per_level, na.rm=T)) }

multiclassPenalty = function(build){
  penalties = 0
  class_levels = g_classLevel(build, g_uniqClassList(build))
  class_levels = class_levels[names(class_levels) %in% g_baseClass()]
  if(!is.null(build$favored_class)){ class_levels = class_levels[!names(class_levels) %in% build$favored_class] }
  class_levels = class_levels[order(class_levels, decreasing=T)]
  if(!is.null(build$favored_class)){
    if(build$favored_class == "Any" & length(class_levels) > 0) class_levels = class_levels[-1]
  }
  if(length(class_levels) > 1){
    penalties = sum(class_levels[1:(length(class_levels)-1)] - class_levels[2:length(class_levels)] > 1)
  }
  return(1 - 0.2*penalties)
}

calculateAverageDamage = function(attacks, damage, defense){
  effective_hit_chance = (attacks - defense + 21) / 20
  effective_hit_chance = sapply(effective_hit_chance, max, 0.05)
  effective_hit_chance = sapply(effective_hit_chance, min, 0.95)
  
  hits = sum(effective_hit_chance)
  
  average_damage = hits * damage
  
  return(average_damage)
}