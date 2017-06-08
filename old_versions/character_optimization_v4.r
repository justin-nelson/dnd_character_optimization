starting_dexterity = 18
starting_intelligence = 18

calculateAverageDamage = function(attacks, damage, defense){
  effective_hit_chance = (attacks - defense + 21) / 20
  effective_hit_chance = sapply(effective_hit_chance, max, 0.05)
  effective_hit_chance = sapply(effective_hit_chance, min, 0.95)
  
  hits = sum(effective_hit_chance)
  
  average_damage = hits * damage
  
  return(average_damage)
}

calculateBAB = function(character_classes){
  monk_levels = sum(character_classes == "monk", na.rm=T)
  high_levels = sum(character_classes != "monk", na.rm=T)
  BAB = high_levels + monk_levels - 1 - floor((monk_levels - 1)/4)
  
  return(BAB)
}

calculateLevel = function(character_classes){ return(sum(!is.na(character_classes))) }

calculateQualifiedClasses = function(avail_classes, character_classes, character_feats){
  qual_class = c()
  if(avail_classes[["fighter"]] > 0){ qual_class = c(qual_class, "fighter") }
  if(avail_classes[["monk"]] > 0){ qual_class = c(qual_class, "monk") }
  if(avail_classes[["swashbuckler"]] > 0) { qual_class = c(qual_class, "swashbuckler") }
  if(all(c("Dodge", "CE") %in% character_feats) 
     & sum(c("WF(Kukri)", "WF(Kama)") %in% character_feats) 
     & calculateBAB(character_classes) >= 5
     & avail_classes[["dervish"]] > 0){
    qual_class = c(qual_class, "dervish")
  }
  if(all(c("WF(Kukri)", "Feint") %in% character_feats)
     & (c("dervish") %in% character_classes || calculateLevel(character_classes) >= 14)
     & avail_classes[["invis_blade"]] > 0){
    qual_class = c(qual_class, "invis_blade")
  }
  if(all(c("WF(Kama)", "Dodge", "Mobility", "WhirlwindAttack", "CE") %in% character_feats)
     & calculateBAB(character_classes) >=5
     & avail_classes[["weapon_master"]] > 0){
    qual_class = c(qual_class, "weapon_master")
  }
  qual_class = names(which(avail_classes[qual_class] > 0))
  if(length(unique(character_classes[!is.na(character_classes)])) == 4){ qual_class = qual_class[qual_class %in% character_classes] }
  if(calculateLevel(character_classes) > 20) qual_class = qual_class[qual_class %in% character_classes]
  if(length(qual_class) == 0) return(c("any"))
  return(qual_class)
}

lastCharacterClass = function(character_classes){ 
  last_class_index = which(is.na(character_classes))[1]-1
  if(is.na(last_class_index)) return(character_classes[length(character_classes)])
  return(character_classes[which(is.na(character_classes))[1]-1]) 
}

calculateFighterLevel = function(character_classes) { return(sum(character_classes == "fighter", na.rm=T)) }
calculateMonkLevel = function(character_classes) { return(sum(character_classes == "monk", na.rm=T)) }
calculateDervishLevel = function(character_classes) { return(sum(character_classes == "dervish", na.rm=T)) }
calculateIBLevel = function(character_classes) { return(sum(character_classes == "invis_blade", na.rm=T)) }
calculateWMLevel = function(character_classes) { return(sum(character_classes == "weapon_master", na.rm=T)) }
calculateSBlevel = function(character_classes) { return(sum(character_classes == "swashbuckler", na.rm=T)) }


prereq_feats = c("CE", "WF(Kukri)", "Feint", "Mobility", "SpringAttack", "WhirlwindAttack")

calculateQualifiedFeats = function(character_classes, character_feats){
  qual_feats = c("TWF", "Dodge", "CE", "PowerfulCharge")
  
  dexterity = starting_dexterity + calculateLevel(character_classes) / 4
  if("GreatDex1" %in% character_feats) dexterity = dexterity + 1
  
  if(calculateBAB(character_classes) >= 1
     & calculateSBlevel(character_classes) == 0) qual_feats = c(qual_feats, c("Weap_Fin"))
  if(calculateBAB(character_classes) >= 1
     & "fighter" %in% character_classes) qual_feats = c(qual_feats, "WF(Kukri)")
  if(calculateBAB(character_classes) >= 1
     & "monk" %in% character_classes) qual_feats = c(qual_feats, "WF(Kama)")
  if("CE" %in% character_feats) qual_feats = c(qual_feats, c("Feint", "Deadly_defense"))
  if(calculateBAB(character_classes) >= 6
     & "TWF" %in% character_feats) qual_feats = c(qual_feats, "ITWF")
  if(calculateBAB(character_classes) >= 11
     & "ITWF" %in% character_feats) qual_feats = c(qual_feats, "GTWF")
  if(calculateLevel(character_classes) >= 21
     & "GTWF" %in% character_feats
     & dexterity >= 25) qual_feats = c(qual_feats, "PTWF")
  if(calculateLevel(character_classes) >= 21) qual_feats = c(qual_feats, c("GreatDex1", "EpicProwess"))
  if(calculateLevel(character_classes) >= 21
     & all(c("EpicProwess", "CE") %in% character_feats)) qual_feats = c(qual_feats, "CombatInsight")
  if(calculateLevel(character_classes) >= 21
     & sum(character_classes == "dervish", na.rm=T) >= 6) qual_feats = c(qual_feats, "EpicCharge")
  if(calculateFighterLevel(character_classes) >= 4
     & "WF(Kama)" %in% character_feats) qual_feats = c(qual_feats, "WS")
  if("Dodge" %in% character_feats) qual_feats = c(qual_feats, "Mobility")
  if(calculateBAB(character_classes) >= 4
     & "Mobility" %in% character_feats) qual_feats = c(qual_feats, "SpringAttack")
  if(calculateBAB(character_classes) >= 4
     & all(c("Dodge", "Mobility", "CE") %in% character_feats)
     & ("SpringAttack" %in% character_feats || calculateDervishLevel(character_classes) >= 3) ) qual_feats = c(qual_feats, "WhirlwindAttack")
  if(calculateFighterLevel(character_classes) >= 8
     & "WF(Kama)" %in% character_feats) qual_feats = c(qual_feats, "GWF")
  if(calculateFighterLevel(character_classes) >= 12
     & all(c("GWF", "WS") %in% character_feats)) qual_feats = c(qual_feats, "GWS")
  if(calculateFighterLevel(character_classes) >= 12) qual_feats = c(qual_feats, "WeaponMastery")
  if(calculateLevel(character_classes) >= 21
     & "GWF" %in% character_feats
     & calculateFighterLevel(character_classes) >= 12) qual_feats = c(qual_feats, "EWF")
  if(calculateLevel(character_classes) >= 21
     & all(c("GWF", "EWF") %in% character_feats)
     & calculateFighterLevel(character_classes) >= 12) qual_feats = c(qual_feats, "EWS")
  if(calculateBAB(character_classes) >= 8) qual_feats = c(qual_feats, "IC")
  qual_feats = qual_feats[!qual_feats %in% character_feats]
  if(length(qual_feats) == 0) return(c("any"))
  return(qual_feats)
}

calculateNumberOfNewFeats = function(character_classes){
  number_of_feats = 0
  if(calculateLevel(character_classes) == 1) number_of_feats = number_of_feats+1
  if(calculateLevel(character_classes) <= 20 & calculateLevel(character_classes) %% 3 == 0) number_of_feats = number_of_feats+1
  if(calculateLevel(character_classes) >  20 & calculateLevel(character_classes) %% 2 == 1) number_of_feats = number_of_feats+1
  if(lastCharacterClass(character_classes) == "fighter" & calculateFighterLevel(character_classes) == 1) number_of_feats = number_of_feats+1
  if(lastCharacterClass(character_classes) == "fighter" & calculateFighterLevel(character_classes) %% 2 == 0) number_of_feats = number_of_feats+1
  return(number_of_feats)
}

calculateAttacks = function(character_classes, character_feats, burst=T){
  BAB = calculateBAB(character_classes)
  attack_bonus = 0
  dexterity = starting_dexterity + calculateLevel(character_classes) / 4
  if("GreatDex1" %in% character_feats) dexterity = dexterity + 1
  main_hand_attacks = c()
  off_hand_attacks = c()
    
  number_of_attacks = max(floor((BAB-1)/5)+1, 1)
  if(calculateMonkLevel(character_classes) >= 1) number_of_attacks = number_of_attacks + 1
  if(calculateMonkLevel(character_classes) >= 11) number_of_attacks = number_of_attacks + 1
  
  number_off_hand_attacks = 1
  if("ITWF" %in% character_feats) number_off_hand_attacks = number_off_hand_attacks + 1
  if("GTWF" %in% character_feats) number_off_hand_attacks = number_off_hand_attacks + 1
  if("PTWF" %in% character_feats) number_off_hand_attacks = number_of_attacks

  if(calculateMonkLevel(character_classes) <= 4) attack_bonus = attack_bonus - 1
  if(calculateMonkLevel(character_classes) <= 8) attack_bonus = attack_bonus - 1
  
  attack_bonus = attack_bonus + 4 #Enhancement Bonus
  
  if(!("TWF" %in% character_feats)) number_off_hand_attacks = 0
  if("Weap_Fin" %in% character_feats
     || calculateSBlevel(character_classes) >= 1) attack_bonus = attack_bonus + floor((dexterity - 10) / 2)
  if("EpicProwess" %in% character_feats) attack_bonus = attack_bonus + 1
  if("WF(Kama)" %in% character_feats) attack_bonus = attack_bonus + 1
  if("GWF" %in% character_feats) attack_bonus = attack_bonus + 1
  if("EWF" %in% character_feats) attack_bonus = attack_bonus + 2
  if("WeaponMastery" %in% character_feats) attack_bonus = attack_bonus + 2
  if("Deadly_defense" %in% character_feats) attack_bonus = attack_bonus - 3

  if(calculateWMLevel(character_classes) >= 5) attack_bonus = attack_bonus + 1
  if(burst) {
    attack_bonus = attack_bonus + floor(((calculateDervishLevel(character_classes) - 1) / 2) + 1)
    if("EpicCharge" %in% character_feats) attack_bonus = attack_bonus + 4
  }

  top_attack_bonus = BAB + attack_bonus
  
  if(calculateMonkLevel(character_classes) >= 1)  { main_hand_attacks = c(main_hand_attacks, top_attack_bonus); number_of_attacks = number_of_attacks - 1; }
  if(calculateMonkLevel(character_classes) >= 11) { main_hand_attacks = c(main_hand_attacks, top_attack_bonus); number_of_attacks = number_of_attacks - 1; }
  main_hand_attacks = c(main_hand_attacks, seq(from=top_attack_bonus, to=top_attack_bonus-5*(number_of_attacks-1), by=-5))
  if(number_off_hand_attacks > 0) off_hand_attacks = c(off_hand_attacks, seq(from=top_attack_bonus, to=top_attack_bonus-5*(number_off_hand_attacks-1), by=-5))
  
  attacks = c(main_hand_attacks, off_hand_attacks)
  return(attacks)
}

calculateDamage = function(character_classes, character_feats, burst=T){
  damage = 0
  crit_chance = 1
  crit_multiplier = 2
  damage = damage + 7/2 # Kama Base Damage
  damage = damage + 4 # Enhancement Bonust

  if("Deadly_defense" %in% character_feats) damage = damage + 5/2
  if("CombatInsight" %in% character_feats) damage = damage + floor((starting_intelligence - 10) / 2)
  if("WS" %in% character_feats) damage = damage + 1
  if("GWS" %in% character_feats) damage = damage + 1
  if("EWS" %in% character_feats) damage = damage + 4
  if("WeaponMastery" %in% character_feats) damage = damage + 2
  if(calculateSBlevel(character_classes) >= 5) damage = damage + floor((starting_intelligence - 10) / 2)
  if(calculateDervishLevel(character_classes) == 10) damage = damage + 14/2
  if(calculateWMLevel(character_classes) >= 5) crit_multiplier = crit_multiplier + 1
  if(calculateWMLevel(character_classes) >= 7) crit_chance = crit_chance + 2
  if("IC" %in% character_feats) crit_chance = crit_chance * 2
  
  if(burst){
    damage = damage + ((calculateDervishLevel(character_classes) - 1) / 2) + 1
    if("EpicCharge" %in% character_feats
       & "PowerfulCharge" %in% character_feats) damage = damage + 9/2
  }
  
  damage = (crit_multiplier * damage * crit_chance + damage * (20-crit_chance)) / 20
  damage = damage + 6 * floor((calculateIBLevel(character_classes) + 1) / 2)
  # damage = damage + 5/2 # Fire weapon
  
  return(damage)
}

calculateScore = function(damage_per_level){ return(sum(1:30 * damage_per_level, na.rm=T)) }

outputCharacter = function(character){
  cat("\n\n")
  char_table = table(character$best_classes)
  cat(paste(names(char_table), char_table,  sep=" ", collapse=" / "))
  cat("\n")
  cat(paste(1:30, ": ", character$best_classes, sep="", collapse=", "))
  cat("\n")
  cat(paste(character$best_feats, collapse=", "))
  cat("\n")
  cat("best_score: ")
  cat(character$best_score)
  cat("\n\n")
  cat("damage by level|")
  cat(paste(1:30, ": ", character$best_damage_per_level, sep="", collapse=", "))
  cat("\n##############################\n")
  cat("burst by level|")
  cat(paste(1:30, ": ", character$best_burst_per_level, sep="", collapse=", "))
  cat("\n##############################\n")
  cat("sustain by level|")
  cat(paste(1:30, ": ", character$best_susta_per_level, sep="", collapse=", "))
  cat("\n\n")
}

build_character = function(avail_classes, cooling_factor_multiplier = 1, burst_weight=0.5, global_mult = 10, pushing_limit = 100, min_cooling = 0.1){
  builds_analyzed = 0
  push = 0
  
  character_classes = rep(NA, 30)
  character_feats = list()
  potential_character_classes = list()
  potential_character_feats = list()
  
  damage_per_level = rep(0, 30)
  burst_per_level = rep(0, 30)
  susta_per_level = rep(0, 30)
  score_per_level = rep(0, 30)
  
  best_damage_per_level = rep(0, 30)
  best_damage_per_level_perm = NA
  best_burst_per_level = rep(0, 30)
  best_susta_per_level = rep(0, 30)
  best_score_per_level = rep(0, 30)
  best_score_per_level_perm = rep(0, 30)
  
  best_classes = NA
  best_feats = NA
  best_score = -1
  
  local_min_level = 50
  level = 0
  start = T
  seenL30 = F
  
  while(length(potential_character_classes) != 0 || start){
    start=F
    
    if(level < local_min_level){
      local_min_level = level
      push = 0
      cat(local_min_level, " / ")
      #best_damage_per_level = damage_per_level
    }
    
    if(level == 30) {
      susta_per_level[30] = 0
      burst_per_level[30] = 0
      damage_per_level[30] = 0 
      score_per_level[30] = 0 
    } else {
      burst_per_level[(level+1):30] = 0
      susta_per_level[(level+1):30] = 0
      damage_per_level[(level+1):30] = 0
      score_per_level[(level+1):30] = 0
    }
    
    push = push + 1
    #if(push %% pushing_limit == 0) cat("Push:", push, " ")
    
    ################################################################
    # Level up the character to 30 so I can properly assess damage
    ##################################################################
    while(level < 30){
      #############################################################
      # Pruning
      ##############################################################
      if(seenL30){
        # Calculate damage
        damage_rolls_burst = calculateDamage(character_classes,  unlist(character_feats), TRUE)
        damage_rolls_susta = calculateDamage(character_classes,  unlist(character_feats), FALSE)
        attack_rolls_burst = calculateAttacks(character_classes, unlist(character_feats), TRUE)
        attack_rolls_susta = calculateAttacks(character_classes, unlist(character_feats), FALSE)
        defense = level + 10
        damage_dealt_burst = calculateAverageDamage(attack_rolls_burst, damage_rolls_burst, defense)
        damage_dealt_susta = calculateAverageDamage(attack_rolls_susta, damage_rolls_susta, defense)
        damage_dealt       = damage_dealt_burst*burst_weight + damage_dealt_susta*(1-burst_weight)
        damage_per_level[level] = damage_dealt
        burst_per_level[level]  = damage_dealt_burst
        susta_per_level[level]  = damage_dealt_susta
        score = calculateScore(damage_per_level)
        score_per_level[level]  = score
        
        local_cooling_factor = 1+min_cooling+( cooling_factor_multiplier / (local_min_level * local_min_level) ) * (push + pushing_limit) / push
        global_cooling_factor = 1+min_cooling+( global_mult * cooling_factor_multiplier / (level * level) ) * (push + pushing_limit) / push
        
        #if(damage_dealt > best_damage_per_level[level]) best_damage_per_level[level] = damage_dealt
        #if(score        > best_score_per_level[level] ) best_score_per_level[level]  = score
        
        if(damage_dealt <= (best_damage_per_level[level] / local_cooling_factor) ) {
          # cat("\n", "level:", level,
          #     "local_dps_break", damage_dealt,
          #     "needed:", (best_damage_per_level[level] / local_cooling_factor),
          #     "best:", best_damage_per_level[level],
          #     "cooling:", local_cooling_factor,
          #     "push:", push)
          break
        }
        if(damage_dealt <= (best_damage_per_level[level] / global_cooling_factor) ) {
          # cat("\n", "level:", level,
          #     "global_dps_break", damage_dealt,
          #     "needed:", (best_damage_per_level[level] / local_cooling_factor),
          #     "best:", best_damage_per_level[level],
          #     "cooling:", global_cooling_factor,
          #     "push:", push)
          break
        }
      }
      
      # If we are about to run into the 3b20 rule, I need to break.
      if(level >= 15 & level < 20){ 
        if(level + sum(sapply(3-table(character_classes), max, 0)) >= 21){ break;}
      }
      
      # Select Class
      if(length(potential_character_classes) < level+1){ 
        potential_character_classes[[level+1]] = calculateQualifiedClasses(avail_classes, character_classes, unlist(character_feats)) 
      }
      class_index = sample(1:length(potential_character_classes[[level+1]]), 1)
      class_name = potential_character_classes[[level+1]][class_index]
      character_classes[level+1] = class_name
      avail_classes[[class_name]] = avail_classes[[class_name]] - 1
      potential_character_classes[[level+1]] = potential_character_classes[[level+1]][-1 * class_index]
      
      # Select Feats
      character_feats[[level+1]] = rep(NA, 3)
      if(length(potential_character_feats) < level+1){
        potential_character_feats[[level+1]] = list()
      }
      numberOfNewFeats = calculateNumberOfNewFeats(character_classes)
      if(numberOfNewFeats >= 1){
        for(i in 1:calculateNumberOfNewFeats(character_classes)){
          if(length(potential_character_feats[[level+1]]) < i){ 
            potential_character_feats[[level+1]][[i]] = calculateQualifiedFeats(character_classes, unlist(character_feats)) 
          }
          probabilities = potential_character_feats[[level+1]][[i]] %in% prereq_feats * 2 + 1
          feat_index = sample(1:length(potential_character_feats[[level+1]][[i]]), 1, prob=probabilities)
          feat_name = potential_character_feats[[level+1]][[i]][feat_index]
          character_feats[[level+1]][i] = feat_name
          potential_character_feats[[level+1]][[i]] = potential_character_feats[[level+1]][[i]][-1 * feat_index]
        }
      }
      
      # Level up
      level = level+1
      #cat(paste(c(rep("#", level), level, "\n"), collapse=""))
      
      # Calculate damage
      damage_rolls_burst = calculateDamage(character_classes,  unlist(character_feats), TRUE)
      damage_rolls_susta = calculateDamage(character_classes,  unlist(character_feats), FALSE)
      attack_rolls_burst = calculateAttacks(character_classes, unlist(character_feats), TRUE)
      attack_rolls_susta = calculateAttacks(character_classes, unlist(character_feats), FALSE)
      defense = level + 10
      damage_dealt_burst = calculateAverageDamage(attack_rolls_burst, damage_rolls_burst, defense)
      damage_dealt_susta = calculateAverageDamage(attack_rolls_susta, damage_rolls_susta, defense)
      damage_dealt       = damage_dealt_burst*burst_weight + damage_dealt_susta*(1-burst_weight)
      damage_per_level[level] = damage_dealt
      burst_per_level[level]  = damage_dealt_burst
      susta_per_level[level]  = damage_dealt_susta
      score = calculateScore(damage_per_level)
      score_per_level[level]  = score
    }
    
    #cat(damage_per_level / best_damage_per_level, "//", local_cooling_factor, "/", global_cooling_factor, "/", push, "\n")
    
    ##########################################
    # Assess
    ##########################################
    if(level == 30){ 
      seenL30 = T
      builds_analyzed = builds_analyzed + 1 
      
      
      damage_rolls_burst = calculateDamage(character_classes,  unlist(character_feats), TRUE)
      damage_rolls_susta = calculateDamage(character_classes,  unlist(character_feats), FALSE)
      attack_rolls_burst = calculateAttacks(character_classes, unlist(character_feats), TRUE)
      attack_rolls_susta = calculateAttacks(character_classes, unlist(character_feats), FALSE)
      defense = level + 10
      damage_dealt_burst = calculateAverageDamage(attack_rolls_burst, damage_rolls_burst, defense)
      damage_dealt_susta = calculateAverageDamage(attack_rolls_susta, damage_rolls_susta, defense)
      damage_dealt       = damage_dealt_burst*burst_weight + damage_dealt_susta*(1-burst_weight)
      damage_per_level[level] = damage_dealt
      burst_per_level[level]  = damage_dealt_burst
      susta_per_level[level]  = damage_dealt_susta
      score = calculateScore(damage_per_level)
      score_per_level[level]  = score
      
      if(score > best_score){
        push = 0
        time_since_last_best = 0
        local_min_level = 50
        best_score   = score
        best_classes = character_classes
        best_feats   = unlist(character_feats)[!is.na(unlist(character_feats))]
        
        best_score_per_level       = score_per_level
        best_score_per_level_perm  = score_per_level
        best_damage_per_level      = damage_per_level
        best_damage_per_level_perm = damage_per_level
        best_burst_per_level       = burst_per_level
        best_susta_per_level       = susta_per_level
        
        cat("\n")
        cat("===== Best Classes so far... =====\n")
        cat(paste(1:30, ": ", character_classes, sep="", collapse=", "))
        #char_table = table(character_classes)
        #cat(paste(names(char_table), char_table,  sep=" ", collapse=" / "))
        cat("\n")
        cat(paste("level 30 damage: ", best_damage_per_level[30], sep=""))
        cat("\n")
        cat(paste("best_score: ", best_score, sep=""))
        cat("\n")
      }
    }
    
    ##########################################
    # Delevel
    ##########################################
    while(level > 0){
      # Delevel Feats
      while(length(potential_character_feats[[level]]) != 0){
        if(length(potential_character_feats[[level]][[length(potential_character_feats[[level]])]]) == 0){
          potential_character_feats[[level]][[length(potential_character_feats[[level]])]] = NULL
        } else {
          break;
        }
      }
      
      if(length(potential_character_feats[[level]]) != 0){
        probabilities = potential_character_feats[[level]][[length(potential_character_feats[[level]])]] %in% prereq_feats * 2 + 1
        feat_index = sample(1:length(potential_character_feats[[level]][[length(potential_character_feats[[level]])]]), 1, prob=probabilities)
        feat_name = potential_character_feats[[level]][[length(potential_character_feats[[level]])]][feat_index]
        character_feats[[level]][length(potential_character_feats[[level]])] = feat_name
        potential_character_feats[[level]][[length(potential_character_feats[[level]])]] = potential_character_feats[[level]][[length(potential_character_feats[[level]])]][-1 * feat_index]
        break
      } else {
        character_feats[[level]] = NA
        potential_character_feats[[level]] = NULL
      }
      
      # Delevel or relevel
      avail_classes[[character_classes[level]]] = avail_classes[[character_classes[level]]] + 1
      if(length(potential_character_classes[[level]]) != 0){
        # Relevel
        class_index = sample(1:length(potential_character_classes[[level]]), 1)
        class_name = potential_character_classes[[level]][class_index]
        character_classes[level] = class_name
        avail_classes[[ class_name ]] = avail_classes[[ class_name ]] - 1
        potential_character_classes[[level]] = potential_character_classes[[level]][-1*class_index]
        
        # Since I releveled, I have to select feats
        character_feats[[level]] = rep(NA, 3)
        if(length(potential_character_feats) < level){
          potential_character_feats[[level]] = list()
        }
        numberOfNewFeats = calculateNumberOfNewFeats(character_classes)
        if(numberOfNewFeats >= 1){
          for(i in 1:calculateNumberOfNewFeats(character_classes)){
            if(length(potential_character_feats[[level]]) < i){ 
              potential_character_feats[[level]][[i]] = calculateQualifiedFeats(character_classes, unlist(character_feats)) 
            }
            probabilities = potential_character_feats[[level]][[i]] %in% prereq_feats * 2 + 1
            feat_index = sample(1:length(potential_character_feats[[level]][[i]]), 1, prob=probabilities)
            character_feats[[level]][i] = potential_character_feats[[level]][[i]][feat_index]
            potential_character_feats[[level]][[i]] = potential_character_feats[[level]][[i]][-1 * feat_index]
          }
        }
        
        break
      } else {
        # Delevel
        character_classes[level] = NA
        potential_character_classes[[level]] = NULL
        level = level - 1
      }
    }
    
  }
  
  return(list(best_classes=best_classes,
              best_damage_per_level=best_damage_per_level_perm,
              best_burst_per_level=best_burst_per_level,
              best_susta_per_level=best_susta_per_level,
              best_feats = best_feats,
              best_score = best_score,
              builds_analyzed = builds_analyzed,
              cooling_factor_multiplier = cooling_factor_multiplier,
              burst_weight = burst_weight))
}


avail_classes = list("monk"=11, "dervish"=10, "invis_blade"=5, "fighter"=30, "weapon_master"=0, "swashbuckler"=5)
min_cooling = 0
cooling_factor_multiplier = 1
pushing_limit = 10
burst_weight = 0.5
global_mult = 3
windmaster_char = build_character(avail_classes, 
                                  cooling_factor_multiplier = cooling_factor_multiplier, 
                                  burst_weight=burst_weight,
                                  global_mult = global_mult,
                                  pushing_limit = pushing_limit,
                                  min_cooling = min_cooling)
outputCharacter(windmaster_char)