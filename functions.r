g_saves = function(build, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  #%%%%%%%%%%%%%%%%%%
  # Stats
  #%%%%%%%%%%%%%%%%%%%%
  saves = c("fortitude"=as.numeric(g_stat_mod(build, "Constitution")),
            "reflex"=as.numeric(g_stat_mod(build, "Dexterity")),
            "will"=as.numeric(g_stat_mod(build, "Wisdom")))
  
  #%%%%%%%%%%%%%%%%
  # Class
  #%%%%%%%%%%%%%%%%%
  for(class_n in g_uniqClassList(build)){
    if(!is.null(cls_av[[class_n]]$fortitude)){
      if(cls_av[[class_n]]$fortitude == "high") saves["fortitude"] = saves["fortitude"] + floor(g_classLevel(build, class_n) / 2 + 2)
      if(cls_av[[class_n]]$fortitude == "low") saves["fortitude"] = saves["fortitude"] + floor(g_classLevel(build, class_n) / 3)
    }
    if(!is.null(cls_av[[class_n]]$reflex)){
      if(cls_av[[class_n]]$reflex == "high") saves["reflex"] = saves["reflex"] + floor(g_classLevel(build, class_n) / 2 + 2)
      if(cls_av[[class_n]]$reflex == "low") saves["reflex"] = saves["reflex"] + floor(g_classLevel(build, class_n) / 3)
    }
    if(!is.null(cls_av[[class_n]]$will)){
      if(cls_av[[class_n]]$will == "high") saves["will"] = saves["will"] + floor(g_classLevel(build, class_n) / 2 + 2)
      if(cls_av[[class_n]]$will == "low") saves["will"] = saves["will"] + floor(g_classLevel(build, class_n) / 3)
    }
  }
  
  #%%%%%%%%%%%%%
  # Feats
  #%%%%%%%%%%%%%%
  for(feat_n in feat_list){
    if(is.null(feats_av[[feat_n]]$saves)) next
    saves = saves + feats_av[[feat_n]]$saves
  }
  
  return(saves)
}

hasAllFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(all(feat_n %in% feat_list)) 
}

hasAnyFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(any(feat_n %in% feat_list)) 
}

g_level = function(build){ 
  if(is.null(build$class)) return(0)
  return(length(g_classList(build)))
}

g_BAB = function(build){
  BAB = 0
  if(g_level(build) == 0) return(0)
  for(class_n in g_uniqClassList(build)){
    classLevel = g_classLevel(build, class_n)
    if(class_n == "any") next
    if(cls_av[[class_n]][["BAB"]] == "high"){ BAB = BAB + classLevel }
    if(cls_av[[class_n]][["BAB"]] == "medium"){ BAB = BAB + floor(0.75*classLevel) }
    if(cls_av[[class_n]][["BAB"]] == "low"){ BAB = BAB + floor(0.5*classLevel) }
  }
  
  return(BAB)
}

gms = 0
g_metamagicString = function(build){ 
  gms <<- gms+1
  mm_list = g_metamagicList(build)
  if(length(mm_list) == 0) return("None")
  return(paste0(mm_list, collapse=" ")) 
}

g_metamagicList = function(build){
  feat_list = g_feats(build)
  metamagic_list = c()
  for(feat_n in feat_list){
    if(is.null(feats_av[[feat_n]])) next
    if(is.null(feats_av[[feat_n]]$isMetamagic)) next
    if(!feats_av[[feat_n]]$isMetamagic) next
    metamagic_list = c(metamagic_list, feat_n)
  }
  return(metamagic_list)
}

g_spellSchools = function(spell_n){
  spellSchools = rep("Universal", length(spell_n))
  for(i in 1:length(spell_n)){
    spellSchools[i] = spells_av[[spell_n[i]]]$spell_school
  }
  return(spellSchools)
}

g_spellDamage = function(damage, spellSuccessRate, savePercentage=0.5){
  success_damage = damage * spellSuccessRate
  failure_damage = damage * (1-spellSuccessRate) * savePercentage
  return(success_damage + failure_damage)
}
g_enemySave = function(build){
  return(g_level(build) * 0.4)
}

g_casterProgression = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  casterProgression = g_classLevel(build, class_n)
  for(feat_n in feat_list){
    if( !is.null(feats_av[[feat_n]]$caster_progression) & !is.null(feats_av[[feat_n]]$class_that_progresses) & !is.null(feats_av[[feat_n]]$class_to_progress) ){
      
      if(feats_av[[feat_n]]$class_to_progress == class_n){
        casterProgression = casterProgression + sum(feats_av[[feat_n]]$caster_progression <= g_classLevel(build, feats_av[[feat_n]]$class_that_progresses))
      }
    }
  }
  
  return(casterProgression)
}

g_casterLevel = function(build, options, class_n, feat_list=NULL, caster_progression=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(caster_progression)) caster_progression = g_casterProgression(build, options, class_n, feat_list)
  caster_level = caster_progression
  return(caster_level)
}

g_spellSuccessRate = function(DC, save){
  return(max(min(DC - save, 19), 1) / 20)
}

g_spellDC = function(build, options, class_n, spell_n){
  base_DC = build$spell_DCs[[class_n]][spells_av[[spell_n]]$spell_school]
  spell_level = 0
  if(!is.null(spells_av[[spell_n]]$spell_level)) spell_level = spells_av[[spell_n]]$spell_level
  DC = base_DC + spell_level
  return(DC)
}
g_baseSpellDC = function(build, options, class_n, school_n, feat_list=NULL, stat_mod=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(stat_mod)) stat_mod = g_stat_mod(build, cls_av[[class_n]]$spell_DC_stat)
  
  spell_school = rep(10+stat_mod, length(school_n))
  names(spell_school) = school_n
  
  for(feat_n in feat_list){
    if(is.null(feats_av[[feat_n]]$DC_bonus)) next
    DC_bonus = feats_av[[feat_n]]$DC_bonus
    DC_bonus = DC_bonus[names(DC_bonus) %in% school_n]
    
    spell_school[names(DC_bonus)] = spell_school[names(DC_bonus)] + DC_bonus
  }
  
  return(spell_school)
}


g_bonusSpellSlotsByLevel = function(stat_mod, level){
  return(max(floor((stat_mod - level)/4)+1, 0))
}

g_bonusSpellSlots = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(cls_av[[class_n]][["spell_stat"]])) return(c())
  spell_stat = cls_av[[class_n]][["spell_stat"]]
  
  stat_mod = g_stat_mod(build, spell_stat, TRUE)
  if("Spellcasting Prodigy" %in% feat_list) stat_mod = stat_mod + 1
  
  bonus_spells = rep(0, 10)
  for(i in 2:10){
    bonus_spells[i] = g_bonusSpellSlotsByLevel(stat_mod, i-1)
  }
  return(bonus_spells)
}

g_spellslots = function(build, options, class_n, feat_list=NULL, caster_progression=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(caster_progression)) caster_progression = g_casterProgression(build, options, class_n, feat_list)
  spell_slots = cls_av[[class_n]][["spells"]][[caster_progression]] 
  spell_slots = spell_slots + g_bonusSpellSlots(build, options, class_n, feat_list)[1:length(spell_slots)]
  return(spell_slots)
}

g_spells = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(build$spell_lists)) return(c())
  
  spell_list = unlist(build$spell_lists[[class_n]])
  
  return(spell_list)
}

g_classLevel = function(build, class_n) {
  classLevels = rep(0, length(class_n))
  for(ind in 1:length(class_n)){
    classLevels[ind] = sum(g_classList(build) == class_n[ind], na.rm=T)
  }
  classLevels[is.na(classLevels)] = 0
  return(classLevels)
}

g_classFeats = function(build, class_n, level){
  if(is.null(cls_av[[class_n]][["feats"]])) return(c())
  feat_list = cls_av[[class_n]][["feats"]][1:level]
  return(unlist(feat_list, F, F))
}

g_grantingFeatList = function(feats_avail){
  feat_list = names(feats_avail)
  granting_feats = c()
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]])){
      if(!is.null(feats_av[[feat_n]][["grant"]])){
        granting_feats = c(granting_feats, feat_n)
      }
    }
  }
  return(granting_feats)
}

g_classList = function(build){ return(build$class[!is.na(build$class)]) }
g_uniqClassList = function(build){ return(unique(g_classList(build))) }

g_feats = function(build){
  c_feats = c()
  ### Get the feats selected at level up
  c_feats = c(c_feats, unlist(build$feats, FALSE, FALSE))
  
  ### Get class feats
  class_list = g_classList(build)
  uniq_class_list = unique(class_list)
  
  for(class_n in uniq_class_list){
    if(!GLOBAL_fastFeatRetrieve){
      if(is.null(cls_av[[class_n]][["feats"]])) next
    }
    class_level = sum(class_list == class_n, na.rm=T)
    if(GLOBAL_fastFeatRetrieve){
      class_level_to_retrieve = min(class_level, length(cls_av[[class_n]][["feats_at_lv"]]))
      feat_list = cls_av[[class_n]][["feats_at_lv"]][[class_level_to_retrieve]]
    } else {
      feat_list = unlist(cls_av[[class_n]][["feats"]][1:class_level])
    }
    c_feats = c(c_feats, feat_list)
  }
  
  ### Get autogrant feats, these are feats that are prereqs for classes and such
  granting_feats = c_feats[c_feats %in% GLOBAL_grantingFeats]
  for(feat_n in granting_feats){
    c_feats = c(c_feats, feats_av[[feat_n]][["grant"]])
  }
  
  if(length(c_feats) > 0) c_feats = c_feats[!is.na(c_feats)]
  
  return(c_feats)
}

g_classSkills = function(class_n){
  if(is.null(cls_av[[class_n]][["class_skills"]])) return(c())
  return(cls_av[[class_n]][["class_skills"]])
}

g_skillMaxes = function(build, skill_n){
  maxSkill = 3+g_level(build)
  minSkill = floor(maxSkill/2)
  skill = rep(minSkill, length(skill_n))
  for(class_n in g_uniqClassList(build)){
    skill[skill_n %in% g_classSkills(class_n)] = maxSkill
  }
  return(skill)
}

g_hasClassSkills = function(build, skill_n){
  skill_list = rep(FALSE, length(skill_n))
  for(class_n in g_uniqClassList(build)){
    skill_list[skill_n %in% g_classSkills(class_n)] = TRUE
  }
  return(skill_list)
}

g_classesWithSkill = function(skill_n){
  class_list = c()
  for(class_n in names(cls_av)){
    if(skill_n %in% g_classSkills(class_n)){
      class_list = c(class_list, class_n)
    }
  }
}

g_prestigeSkillRequirements = function(build, prestigeSearch=""){
  if(prestigeSearch == "") return(c())
  if(is.null(cls_av[[prestigeSearch]])) return(c())
  if(is.null(cls_av[[prestigeSearch]][["prereq_skills"]])) return(c())
  
  prereq_skills = cls_av[[prestigeSearch]][["prereq_skills"]]
  return(prereq_skills)
}


g_scoreFeats = function(build, feat_list, options=list()){
  scores = rep(0, length(feat_list))
  for(ind in 1:length(feat_list)){
    feat_n = feat_list[ind]
    scoring_build = build
    scoring_build$feats[[length(scoring_build$feats)+1]] = c(feat_n)
    global_evaluations_4 <<- global_evaluations_4 + 1
    scores[ind] = evaluateBuild(scoring_build, options, g_level(scoring_build), 1)$score
  }
  return(scores)
}

g_orderedBestFeats = function(build, options=list()){
  feat_list = getChoosableFeatList(feats_av)
  scores = g_scoreFeats(build, feat_list, options)
  feat_list = feat_list[order(scores, decreasing=T)]
  scores = scores[order(scores, decreasing=T)]
  return(feat_list)
}



g_featsInFeatType = function(feat_type){
  feat_list = c()
  for(feat_n in names(feats_av)){
    if( is.null(feats_av[[feat_n]][["feat_type"]])        ) next
    if( !feat_type %in% feats_av[[feat_n]][["feat_type"]] ) next
    feat_list = c(feat_list, feat_n)
  }
  return(feat_list)
}

g_featPrereqs = function(feat_n){
  prereqs = c()
  if(is.null(feats_av[[feat_n]])) return(prereqs)
  if(is.null(feats_av[[feat_n]][["prereq_feats"]])) return(prereqs)
  
  prereqs = feats_av[[feat_n]][["prereq_feats"]]
  if(length(prereqs) > 0){
    for(feat_n2 in prereqs){
      prereqs = c(prereqs, g_featPrereqs(feat_n2))
    }
  }
  
  return(prereqs)
}

g_prestigeFeatRequirements = function(build, prestigeSearch=""){
  if(prestigeSearch == "") return(c())
  if(is.null(cls_av[[prestigeSearch]])) return(c())
  if(is.null(cls_av[[prestigeSearch]][["prereq_feats"]])) return(c())
  
  prereq_feats = cls_av[[prestigeSearch]][["prereq_feats"]]
  prereq_feats_fin = prereq_feats
  for(feat_n in prereq_feats){
    prereq_feats_fin = c(prereq_feats_fin, g_featPrereqs(feat_n))
  }
  
  prereq_feats = unique(prereq_feats_fin)
  prereq_feats = prereq_feats[!prereq_feats %in% g_feats(build)]
  
  return(prereq_feats)
}

g_stat_enhancement = function(build, options, stat, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  equipment_bonus = build$equipment_stats
  enhancement_bonus = equipment_bonus
  for(feat_n in feat_list){
    if(is.null(feats_av[[feat_n]])) next
    if(is.null(feats_av[[feat_n]]$enhancement_bonus)) next
    spell_bonus = feats_av[[feat_n]]$enhancement_bonus(build, options, feat_list, equipment_bonus)
    enhancement_bonus = pmax(enhancement_bonus, spell_bonus)
  }
  return(enhancement_bonus[stat])
}

g_stats = function(build, options, stats, feat_list=NULL, useSpells=F){
  if(is.null(feat_list)) feat_list = g_feats(build)
  base_stats = build$base_stats[stats]
  for(stat_n in names(base_stats)){
    base_stats[stat_n] = base_stats[stat_n] + sum(feat_list %in% feats_to_increase_stats[[stat_n]])
  }
  if(useSpells) base_stats = base_stats + g_stat_enhancement(build, options, stats, feat_list)
  if(!is.null(build$stat_increase)){ base_stats[build$stat_increase] = base_stats[build$stat_increase] + floor(g_level(build) / 4) }
  return(unlist(base_stats[stats], F, F))
}

g_inherentValue = function(feat_list){
  inherent_value = 0
  for(feat_n in feat_list){
    if( !is.null(feats_av[[feat_n]]$inherent_value) ) inherent_value = inherent_value + feats_av[[feat_n]]$inherent_value
  }
  return(inherent_value)
}

g_stat_mod = function(build, stat_n, useRest=F){
  stats = build$stats[stat_n]
  if(useRest) stats = build$rest_stats[stat_n]
  return(floor((stats - 10) / 2))
}

g_baseClass = function(){
  base_class_list = c()
  for(class_n in names(cls_av)){
    if(is.null(cls_av[[class_n]][["base"]])) next
    if(!cls_av[[class_n]][["base"]]) next
    base_class_list = c(base_class_list, class_n)
  }
  return(base_class_list)
}

g_aveDamageDice = function(dice, sides){ return((dice + dice*sides)/2) }

g_concealment = function(build, options, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  concealment = 0
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["concealment"]])){
      concealment = max(concealment, feats_av[[feat_n]][["concealment"]](build, options, feat_list))
    }
  }
  
  return(concealment)
}

g_DR = function(build, options, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  DR = 0
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["DR"]])){
      DR = DR + feats_av[[feat_n]][["DR"]](build, options, feat_list)
    }
  }
  
  return(DR)
}
g_hitsAbsorbed = function(build, options, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  hits_absorbed = 0
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["hits_absorbed"]])){
      hits_absorbed = hits_absorbed + feats_av[[feat_n]]$hits_absorbed(build, options, feat_list)
    }
  }
  return(hits_absorbed)
}

g_HP = function(build, options, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  HP = g_stat_mod(build, "Constitution") * g_level(build)
  
  class_list = g_classList(build)
  for(class_n in g_uniqClassList(build)){
    if(is.null(cls_av[[class_n]]$hit_die)) next
    HP = HP + cls_av[[class_n]]$hit_die * sum(class_list == class_n, na.rm=T)
  }
  
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["HP"]])){
      HP = HP + feats_av[[feat_n]][["HP"]](build, options, feat_list)
    }
  }
  
  return(HP)
}

g_wearableArmorTypes = function(build, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  wearableArmorTypes = c("None")
  if(hasAnyFeats(build, "Armor Proficiency (Light)")) wearableArmorTypes = c(wearableArmorTypes, "Light")
  if(hasAnyFeats(build, "Armor Proficiency (Medium)")) wearableArmorTypes = c(wearableArmorTypes, "Medium")
  if(hasAnyFeats(build, "Armor Proficiency (Heavy)")) wearableArmorTypes = c(wearableArmorTypes, "Heavy")
  return(wearableArmorTypes)
}

g_AC = function(build, options, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  dex_mod = g_stat_mod(build, "Dexterity")
  best_AC = 0
  
  wearableArmorTypes = g_wearableArmorTypes(build, feat_list)
  
  for(armorType in wearableArmorTypes){
    options$currentArmorType = armorType
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Initialize Armor Bonuses
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    armor_bonus = 0
    deflection_bonus = 0
    natural_bonus = 0
    shield_bonus = 0
    
    dodge_bonus = 0
    untyped_bonus = 0
    shield_AC = 0
    
    if(!is.null(build$armor)){
      if(!is.null(build$armor$armor_bonus)) armor_bonus = build$armor$armor_bonus
      if(!is.null(build$armor$deflection_bonus)) deflection_bonus = build$armor$deflection_bonus
      if(!is.null(build$armor$natural_bonus)) natural_bonus = build$armor$natural_bonus
      if(!is.null(build$armor$shield_bonus)) shield_bonus = build$armor$shield_bonus
      if(!is.null(build$armor$dodge_bonus)) dodge_bonus = build$armor$dodge_bonus
    }
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Get the best shield to equip
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if(build$weapon$hands == "Weapon-and-Shield" & hasAllFeats(build, "Shield Proficiency", feat_list)){
      shield_AC = 2
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Get the best armor of the armorType to equip
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if(!is.null(armors_av[[armorType]])){
      possibleArmors = names(armors_av[[armorType]])
      
      best_armor_AC = 0
      for(armor in possibleArmors){
        options$currentArmor = armor
        armor_AC = armors_av[[armorType]][[armor]]$armor_class + min(dex_mod, armors_av[[armorType]][[armor]]$max_dex)
        if(armor_AC > best_armor_AC){
          best_armor_AC = armor_AC
          options$bestArmor = armor
        }
      }
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Check for bonuses from feat, based on currentArmorType
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for(feat_n in feat_list){
      if(!is.null(feats_av[[feat_n]][["armor_bonus"]])){
        armor_bonus = max(armor_bonus, feats_av[[feat_n]][["armor_bonus"]](build, options, feat_list))
      }
      
      if(!is.null(feats_av[[feat_n]][["deflection_bonus"]])){
        deflection_bonus = max(deflection_bonus, feats_av[[feat_n]][["deflection_bonus"]](build, options, feat_list))
      }
      if(!is.null(feats_av[[feat_n]][["natural_bonus"]])){
        natural_bonus = max(natural_bonus, feats_av[[feat_n]][["natural_bonus"]](build, options, feat_list))
      }
      if(!is.null(feats_av[[feat_n]][["shield_bonus"]])){
        shield_bonus = max(shield_bonus, feats_av[[feat_n]][["shield_bonus"]](build, options, feat_list))
      }
      if(!is.null(feats_av[[feat_n]][["dodge_bonus"]])){
        dodge_bonus = dodge_bonus + feats_av[[feat_n]][["dodge_bonus"]](build, options, feat_list)
      }
      if(!is.null(feats_av[[feat_n]][["untyped_bonus"]])){
        untyped_bonus = untyped_bonus + feats_av[[feat_n]][["untyped_bonus"]](build, options, feat_list)
      }
    }
    
    current_AC = 10 + best_armor_AC + shield_AC + armor_bonus + deflection_bonus + natural_bonus + shield_bonus + min(dodge_bonus, 20) + untyped_bonus
    if(best_AC < current_AC) {
      best_AC = current_AC
      best_armorType = armorType
    }
    
  }
  
  return(list(AC=best_AC,
              armorType=best_armorType))
}

g_chanceToHit = function(attacks, defense){
  effective_hit_chance = (attacks - defense + 21) / 20
  effective_hit_chance = sapply(effective_hit_chance, max, 0.05)
  effective_hit_chance = sapply(effective_hit_chance, min, 0.95)
  
  return(effective_hit_chance)
}