CHARGING_RATIO = 0.25
FIGHTING_RATIO = 0.75
FEINTING_RATIO = 0.80

feats_av = list()
# feats_av[["Template"]] = list(name="Template",
#                               feat_type=c("Regular", "Fighter"),
#                               prereq_level=31,
#                               prereq_BAB=31,
#                               prereq_stat=list("Strength"=100,
#                                                "Dexterity"=100,
#                                                "Constitution"=100,
#                                                "Intelligence"=100,
#                                                "Wisdom"=100,
#                                                "Charisma"=100),
#                               prereq_feats=c("Dodge"),
#                               prereq_skills=list("any"=100),
#                               prereq_class=list("Fighter"=100),
#                               grant=c("Template 2"),
#                               num_attacks_off=function(build, options, feat_list){},
#                               attack_bonus=function(build, options, feat_list){},
#                               damage=function(build, options, feat_list){},
#                               postCrit_damage=function(build, options, feat_list){},
#                               plusStrength=1,
#                               plusDexterity=1,
#                               plusConstitution=1,
#                               plusIntelligence=1,
#                               plusWisdom=1,
#                               plusCharisma=1)

PLUSONE = function(build, options, feat_list){ return(1) }

TWO_WEAPON_FIGHTING_OFF_ATTACKS = function(build, options, feat_list){ 
  if(build$weapon$hands == "Two-Weapon") return(1)
  return(0)
}

feats_av[["Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_stat=list("Dexterity"=15),
                                         num_attacks_off=TWO_WEAPON_FIGHTING_OFF_ATTACKS)
feats_av[["Improved Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=6,
                                                  prereq_stat=list("Dexterity"=17),
                                                  prereq_feats=c("Two-Weapon Fighting"),
                                                  num_attacks_off=TWO_WEAPON_FIGHTING_OFF_ATTACKS)
feats_av[["Greater Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_level=0,
                                                 prereq_BAB=11,
                                                 prereq_stat=list("Dexterity"=19),
                                                 prereq_feats=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting"),
                                                 num_attacks_off=TWO_WEAPON_FIGHTING_OFF_ATTACKS)
feats_av[["Perfect Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_level=21,
                                                 prereq_stat=list("Dexterity"=25),
                                                 prereq_feats=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting", "Greater Two-Weapon Fighting"))

feats_av[["Dodge"]] = list(feat_type=c("Regular", "Fighter"),
                           prereq_stat=list("Dexterity"=13),
                           dodge_bonus=PLUSONE)
feats_av[["Mobility"]] = list(feat_type=c("Regular", "Fighter"),
                              prereq_stat=list("Dexterity"=13),
                              prereq_feats=c("Dodge"))
feats_av[["Spring Attack"]] = list(feat_type=c("Regular", "Fighter"),
                                   prereq_stat=list("Dexterity"=13),
                                   prereq_feats=c("Dodge", "Mobility"),
                                   prereq_BAB=4)
feats_av[["Combat Expertise"]] = list(feat_type=c("Regular", "Fighter"),
                                      prereq_stat=list("Intelligence"=13),
                                      attack_bonus=function(build, options, feat_list){ 
                                        if(!is.null(options$noExpertise)){
                                          if(options$noExpertise) return(0)
                                        } 
                                        return(-3)
                                      },
                                      untyped_bonus=function(build, options, feat_list){ 
                                        if(!is.null(options$noExpertise)){
                                          if(options$noExpertise) return(0)
                                        } 
                                        return(3)
                                      })
feats_av[["Feint"]] = list(feat_type=c("Regular"),
                           prereq_stat=list("Intelligence"=13),
                           prereq_feats=c("Combat Expertise"))
feats_av[["Whirlwind Attack"]] = list(feat_type=c("Regular", "Fighter"),
                                      prereq_stat=list("Dexterity"=13,
                                                       "Intelligence"=13),
                                      prereq_feats=c("Dodge", "Mobility", "Spring Attack", "Combat Expertise"),
                                      prereq_BAB=4)
feats_av[["Weapon Focus (Kukri)"]] = list(feat_type=c("Regular", "Fighter"),
                                          prereq_BAB=1,
                                          prereq_feats=c("Weapon Proficiency (Martial)"),
                                          grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)", "Weapon Focus (Invisible Blade)"),
                                          attack_bonus=function(build, options, feat_list){ 
                                            if(build$weapon$name == "Kukri") { return(1) } else { return(0) }
                                          })

KAMA_PLUSONE = function(build, options, feat_list){ 
  if(build$weapon$name == "Kama") { return(1) } else { return(0) }
}
feats_av[["Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_BAB=1,
                                         prereq_feats=c("Weapon Proficiency (Monk)"),
                                         grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)"),
                                         attack_bonus=KAMA_PLUSONE)
feats_av[["Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=4,
                                                  prereq_feats=c("Weapon Focus (Kama)"),
                                                  prereq_class=list("Fighter"=4),
                                                  damage=KAMA_PLUSONE)
feats_av[["Greater Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_feats=c("Weapon Focus (Kama)"),
                                                 prereq_class=list("Fighter"=8),
                                                 attack_bonus=KAMA_PLUSONE)
feats_av[["Greater Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                          prereq_feats=c("Weapon Specialization (Kama)", "Greater Weapon Focus (Kama)"),
                                                          prereq_class=list("Fighter"=12),
                                                          damage=KAMA_PLUSONE)
feats_av[["Epic Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                              prereq_level=21,
                                              prereq_feats=c("Greater Weapon Focus (Kama)"),
                                              attack_bonus=function(build, options, feat_list){ 
                                                if(build$weapon$name == "Kama") { return(2) } else { return(0) }
                                              })
feats_av[["Epic Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                       prereq_level=21,
                                                       prereq_feats=c("Epic Weapon Focus (Kama)", "Greater Weapon Specialization (Kama)"),
                                                       damage=function(build, options, feat_list){ 
                                                         if(build$weapon$name == "Kama") { return(4) } else { return(0) }
                                                       })
feats_av[["Melee Weapon Mastery (Slashing)"]] = list(feat_type=c("Fighter"),
                                                     prereq_class=list("Fighter"=12),
                                                     damage=function(build, options, feat_list){ 
                                                       if(build$weapon$damage_type == "Slashing") { return(2) } else { return(0) }
                                                     },
                                                     attack_bonus=function(build, options, feat_list){ 
                                                       if(build$weapon$damage_type == "Slashing") { return(2) } else { return(0) }
                                                     })
feats_av[["Improved Critical (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                              prereq_BAB=8,
                                              prereq_feats=c("Weapon Proficiency (Monk)"),
                                              crit_chance_multiplier=function(build, options, feat_list){
                                                if(build$weapon$name == "Kama") { return(2) } else { return(1) }
                                              })

feats_av[["Weapon Finesse"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_BAB=1,
                                    attack_bonus=function(build, options, feat_list){ 
                                      if(build$weapon$size != "Light" & build$weapon$name != "Rapier") return(0)
                                      if(g_stat_mod(build, "Dexterity", feat_list, TRUE) > g_stat_mod(build, "Strength", feat_list, TRUE)){
                                        return(g_stat_mod(build, "Dexterity", feat_list, TRUE) - g_stat_mod(build, "Strength", feat_list, TRUE))
                                      }
                                      return(0)
                                    })
feats_av[["Epic Prowess"]] = list(feat_type=c("Regular", "Fighter"),
                                  prereq_level=21,
                                  attack_bonus=PLUSONE)
feats_av[["Great Dexterity 1"]] = list(feat_type=c("Regular"),
                                       prereq_level=21,
                                       plusDexterity=1)
feats_av[["Great Dexterity 2"]] = list(feat_type=c("Regular"),
                                       prereq_level=21,
                                       prereq_feats=c("Great Dexterity 1"),
                                       plusDexterity=1)
feats_av[["Great Dexterity 3"]] = list(feat_type=c("Regular"),
                                       prereq_level=21,
                                       prereq_feats=c("Great Dexterity 2"),
                                       plusDexterity=1)
feats_av[["Great Dexterity 4"]] = list(feat_type=c("Regular"),
                                       prereq_level=21,
                                       prereq_feats=c("Great Dexterity 3"),
                                       plusDexterity=1)
feats_av[["Great Dexterity 5"]] = list(feat_type=c("Regular"),
                                       prereq_level=21,
                                       prereq_feats=c("Great Dexterity 4"),
                                       plusDexterity=1)
feats_av[["Improved Initiative"]] = list(feat_type=c("Regular"))
feats_av[["Powerful Charge"]] = list(feat_type=c("Regular"),
                                     damage=function(build, options, feat_list){
                                       if(is.null(options$allow_burst)) return(0)
                                       if(!options$allow_burst) return(0)
                                       if(hasAllFeats(build, "Dire Charge", feat_list)){ return(g_aveDamageDice(2, 6) * CHARGING_RATIO) }
                                       return(0)
                                     })
feats_av[["Dire Charge"]] = list(feat_type=c("Regular"),
                                 prereq_level=21,
                                 prereq_feats=c("Improved Initiative"),
                                 attack_bonus=function(build, options, feat_list){ 
                                   if(is.null(options$allow_burst)) return(0)
                                   if(!options$allow_burst) return(0)
                                   return(4 * CHARGING_RATIO) 
                                 })
feats_av[["Deadly Defense"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_feats=c("Combat Expertise"),
                                    damage=function(build, options, feat_list){return(g_aveDamageDice(1,4))})
feats_av[["Combat Insight"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_level=21,
                                    prereq_feats=c("Combat Expertise", "Epic Prowess"),
                                    damage=function(build, options, feat_list){
                                      if(g_stat_mod(build, "Intelligence", feat_list, TRUE) > g_stat_mod(build, "Strength", feat_list, TRUE)){
                                        return(g_stat_mod(build, "Intelligence", feat_list, TRUE) - g_stat_mod(build, "Strength", feat_list, TRUE))
                                      }
                                      return(0)
                                    })
feats_av[["Insightful Strike"]] = list(feat_type=c(),
                                       damage=function(build, options, feat_list){
                                         if(g_stat_mod(build, "Intelligence", feat_list, TRUE) > 0){
                                           return(g_stat_mod(build, "Intelligence", feat_list, TRUE))
                                         }
                                         return(0)
                                       })

DERVISH_DANCE = function(build, options, feat_list){ 
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  bonus = floor((g_classLevel(build, "Dervish")+1)/2)
  boost_ratio = min((bonus * g_skillMaxes(build, "perform") / 20) / (calculateLevel(build) * FIGHTING_RATIO), 1)
  return(bonus * boost_ratio)
}
feats_av[["Dervish Dance"]] = list(feat_type=c(),
                                   damage=DERVISH_DANCE,
                                   attack_bonus=DERVISH_DANCE)

feats_av[["A Thousand Cuts"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_burst)) return(0)
                                       if(!options$allow_burst) return(0)
                                       boost_ratio = min(1.5 / (calculateLevel(build) * FIGHTING_RATIO), 1)
                                       return(boost_ratio * g_aveDamageDice(2, 6))
                                     })

BLEEDING_WOUND = function(build, options, feat_list){
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  if(build$weapon$size != "Light") return(0)
  bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma", feat_list, TRUE)
  if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
  BLUFF_RATIO = max(min((bluff_skill - calculateLevel(build) + 10) / 20, 0.95), 0.05)
  SNEAK_RATIO = 0.2
  if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
  SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
  return(6 * SA_RATIO)
}
feats_av[["Bleeding Wound I"]] = list(feat_type=c(),
                                      postCrit_damage=BLEEDING_WOUND)
feats_av[["Bleeding Wound II"]] = list(feat_type=c(),
                                       postCrit_damage=BLEEDING_WOUND)
feats_av[["Bleeding Wound III"]] = list(feat_type=c(),
                                        postCrit_damage=BLEEDING_WOUND)
feats_av[["Unfettered Defense"]] = list(feat_type=c(),
                                        dodge_bonus=function(build, options, feat_list){ 
                                          # Do I wear no armor?
                                          armorPass = TRUE
                                          if(!is.null(options$currentArmorType)) armorPass = (options$currentArmorType %in% c("None"))
                                          
                                          # Am I not using a ranged weapon?
                                          weaponPass = FALSE
                                          if(!is.null(build$weapon$hands)) weaponPass = (build$weapon$hands != "Ranged")
                                          
                                          if(armorPass & weaponPass) return(max(g_stat_mod(build, "Intelligence", feat_list), g_classLevel(build, "Invisible Blade")))
                                          return(0)
                                        })
feats_av[["Weapon Focus (Slashing)"]] = list(feat_type=c(),
                                             prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Any)"]] = list(feat_type=c(),
                                        prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Invisible Blade)"]] = list(feat_type=c(),
                                                    prereq_feats=c("Weapon Focus (Kukri)"))


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
feats_av[["Tempest Defense I"]] = list(feat_type=c(),
                                       dodge_bonus=TEMPEST_DEFENSE_FUNCTION)
feats_av[["Tempest Defense II"]] = list(feat_type=c(),
                                        attack_bonus=TEMPEST_DEFENSE_FUNCTION)
feats_av[["Tempest Defense III"]] = list(feat_type=c())
feats_av[["Tempest Defense IV"]] = list(feat_type=c(),
                                        dodge_bonus=TEMPEST_DEFENSE_FUNCTION)
feats_av[["Tempest Defense V"]] = list(feat_type=c(),
                                       attack_bonus=TEMPEST_DEFENSE_FUNCTION)

feats_av[["Wounding Critical"]] = list(feat_type=c(),
                                       onCrit_damage=function(build, options, feat_list){
                                         return(calculateLevel(build))
                                       })

feats_av[["Two-Weapon Style"]] = list(feat_type=c("Ranger"))
feats_av[["Ranger Two-Weapon Fighting"]] = list(feat_type=c("Ranger"),
                                                grant=c("Two-Weapon Fighting"),
                                                prereq_feats=c("Two-Weapon Style"))
feats_av[["Ranger Improved Two-Weapon Fighting"]] = list(feat_type=c("Ranger"),
                                                         grant=c("Improved Two-Weapon Fighting"),
                                                         prereq_feats=c("Ranger Two-Weapon Fighting"))
feats_av[["Ranger Greater Two-Weapon Fighting"]] = list(feat_type=c("Ranger"),
                                                        grant=c("Greater Two-Weapon Fighting"),
                                                        prereq_feats=c("Ranger Improved Two-Weapon Fighting"))
feats_av[["Ranger Perfect Two-Weapon Fighting"]] = list(feat_type=c("Ranger"),
                                                        grant=c("Perfect Two-Weapon Fighting"),
                                                        prereq_feats=c("Ranger Greater Two-Weapon Fighting"))
feats_av[["Great Strength 1"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      plusStrength=1)
feats_av[["Great Strength 2"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Dexterity 1"),
                                      plusStrength=1)
feats_av[["Great Strength 3"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Dexterity 2"),
                                      plusStrength=1)
feats_av[["Great Strength 4"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Dexterity 3"),
                                      plusStrength=1)
feats_av[["Great Strength 5"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Dexterity 4"),
                                      plusStrength=1)

SNEAK_ATTACK = function(build, options, feat_list){
  if(is.null(options$allow_burst)) return(0)
  if(!options$allow_burst) return(0)
  bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma", feat_list, TRUE)
  if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
  BLUFF_RATIO = max(min((bluff_skill - calculateLevel(build) + 10) / 20, 0.95), 0.05)
  SNEAK_RATIO = 0.2
  if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
  SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
  return(g_aveDamageDice(1,6) * SA_RATIO)
}
feats_av[["Sneak Attack +1d6"]] = list(feat_type=c(),
                                       damage=SNEAK_ATTACK)
feats_av[["Sneak Attack +2d6"]] = list(feat_type=c(),
                                       damage=SNEAK_ATTACK)
feats_av[["Sneak Attack +3d6"]] = list(feat_type=c(),
                                       damage=SNEAK_ATTACK)
feats_av[["Critical Sense 1"]] = list(feat_type=c(),
                                      attack_bonus=PLUSONE)
feats_av[["Critical Sense 2"]] = list(feat_type=c(),
                                      attack_bonus=PLUSONE)

FAVORED_ENEMY_DAMAGE = function(build, options, feat_list){ 
  if(is.null(options$allow_favoredEnemy)) return(0)
  if(!options$allow_favoredEnemy) return(0)
  return(1)
}

feats_av[["Favored Enemy I"]] = list(feat_type=c(),
                                     damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Favored Enemy II"]] = list(feat_type=c(),
                                      damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Favored Enemy III"]] = list(feat_type=c(),
                                       damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Favored Enemy IV"]] = list(feat_type=c(),
                                      damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Favored Enemy V"]] = list(feat_type=c(),
                                     damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Favored Enemy VI"]] = list(feat_type=c(),
                                      damage=FAVORED_ENEMY_DAMAGE)
feats_av[["Bane of Enemies"]] = list(feat_type=c("Regular"),
                                     prereq_level=21,
                                     prereq_class=list("Ranger"=21),
                                     attack_bonus=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(2)
                                     },
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(g_aveDamageDice(2,6))
                                     })
feats_av[["Monk AC Bonus"]] = list(feat_type=c(),
                                   dodge_bonus=function(build, options, feat_list){ 
                                     # Do I wear no armor?
                                     armorPass = TRUE
                                     if(!is.null(options$currentArmorType)) armorPass = options$currentArmorType %in% c("None")
                                     if(armorPass) return(g_stat_mod(build, "Wisdom", feat_list))
                                     return(0)
                                   })

MONK_AC = function(build, options, feat_list){ 
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = options$currentArmorType %in% c("None")
  if(armorPass) return(1)
  return(0)
}

feats_av[["Monk AC I"]] = list(feat_type=c(),
                               dodge_bonus=MONK_AC)
feats_av[["Monk AC II"]] = list(feat_type=c(),
                                dodge_bonus=MONK_AC)
feats_av[["Monk AC III"]] = list(feat_type=c(),
                                 dodge_bonus=MONK_AC)
feats_av[["Monk AC IV"]] = list(feat_type=c(),
                                dodge_bonus=MONK_AC)
feats_av[["Monk AC V"]] = list(feat_type=c(),
                               dodge_bonus=MONK_AC)
feats_av[["Monk AC VI"]] = list(feat_type=c(),
                                dodge_bonus=MONK_AC)

SWASHBUCKLER_AC = function(build, options, feat_list){ 
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = (options$currentArmorType %in% c("None", "Light"))
  if(armorPass) return(1)
  return(0)
}

feats_av[["Swashbuckler Dodge I"]] = list(feat_type=c(),
                                          dodge_bonus=SWASHBUCKLER_AC)
feats_av[["Swashbuckler Dodge II"]] = list(feat_type=c(),
                                           dodge_bonus=SWASHBUCKLER_AC)
feats_av[["Swashbuckler Dodge III"]] = list(feat_type=c(),
                                            dodge_bonus=SWASHBUCKLER_AC)
feats_av[["Swashbuckler Dodge IV"]] = list(feat_type=c(),
                                           dodge_bonus=SWASHBUCKLER_AC)
feats_av[["Swashbuckler Dodge V"]] = list(feat_type=c(),
                                          dodge_bonus=SWASHBUCKLER_AC)
feats_av[["Swashbuckler Dodge VI"]] = list(feat_type=c(),
                                           dodge_bonus=SWASHBUCKLER_AC)
DERVISH_AC = function(build, options, feat_list){ 
  if(build$weapon$hands == "Weapon-and-Shield") return(0)
  # Do I wear no armor?
  armorPass = TRUE
  if(!is.null(options$currentArmorType)) armorPass = (options$currentArmorType %in% c("None", "Light"))
  if(armorPass) return(1)
  return(0)
}
feats_av[["Dervish AC Bonus I"]] = list(feat_type=c(),
                                        dodge_bonus=DERVISH_AC)
feats_av[["Dervish AC Bonus II"]] = list(feat_type=c(),
                                         dodge_bonus=DERVISH_AC)
feats_av[["Dervish AC Bonus III"]] = list(feat_type=c(),
                                          dodge_bonus=DERVISH_AC)


SPELLCASTING_DAMAGE = function(build, options, feat_list, class_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(spell_list_id)]
  
  damage = 0
  for(spell_n in uniq_spell_list){
    spell_id = paste(names(spell_n), spell_n)
    metamagic = names(spell_id)
    if(is.null(metamagic)) metamagic = ""
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$damage)) next
    
    number_of_times_cast = sum(spell_list_id == spell_id, na.rm=T)
    
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic) * number_of_times_cast
    DURATION_RATIO = min(time / (6* calculateLevel(build)), 1)
    
    damage = damage + spells_av[[spell_n]]$damage(build, options, feat_list, class_n, metamagic) * DURATION_RATIO
  }
  
  return(damage)
}

SPELLCASTING_ENHANCEMENT_BONUS = function(build, options, feat_list, class_n, stable_bonus){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(spell_list_id)]
  
  enhancement_bonus = stable_bonus
  
  for(spell_n in uniq_spell_list){
    spell_id = paste(names(spell_n), spell_n)
    metamagic = names(spell_id)
    if(is.null(metamagic)) metamagic = ""
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$enhancement_bonus)) next
    
    number_of_times_cast = sum(spell_list_id == spell_id, na.rm=T)
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic) * number_of_times_cast
    DURATION_RATIO = min(time / (6* calculateLevel(build)), 1)
    
    spell_EB = spells_av[[spell_n]]$enhancement_bonus(build, options, feat_list, class_n, metamagic)
    spell_EB = enhancement_bonus + (enhancement_bonus - spell_EB) * DURATION_RATIO
    
    enhancement_bonus = pmax(enhancement_bonus, spell_EB)
  }
  
  return(round(enhancement_bonus))
}

SPELLCASTING_FIXED_DAMAGE = function(build, options, feat_list, class_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(spell_list_id)]
  
  fixed_damage = c()
  for(spell_n in uniq_spell_list){
    spell_id = paste(names(spell_n), spell_n)
    metamagic = names(spell_id)
    if(is.null(metamagic)) metamagic = ""
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]]$fixed_damage)) next
    
    number_of_times_cast = sum(spell_list_id == spell_id, na.rm=T)
    spell_damage = spells_av[[spell_n]]$fixed_damage(build, options, feat_list, class_n, metamagic)
    fixed_damage = c(fixed_damage, rep(spell_damage, number_of_times_cast))
    
  }
  
  return(fixed_damage)
}

SPELLCASTING_MAX_DURATION_TRAIT = function(build, options, feat_list, class_n, trait_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(spell_list_id)]
  
  trait = 0
  for(spell_n in uniq_spell_list){
    spell_id = paste(names(spell_n), spell_n)
    metamagic = names(spell_id)
    if(is.null(metamagic)) metamagic = ""
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]][[trait_n]])) next
    
    number_of_times_cast = sum(spell_list_id == spell_id, na.rm=T)
    time = spells_av[[spell_n]]$time(build, options, feat_list, class_n, metamagic) * number_of_times_cast
    DURATION_RATIO = min(time / (6* calculateLevel(build)), 1)
    
    new_trait = spells_av[[spell_n]][[trait_n]](build, options, feat_list, class_n, metamagic)
    
    trait = max(trait, new_trait)
  }
  
  return(trait)
}

SPELLCASTING_CUMULATIVE_TRAIT = function(build, options, feat_list, class_n, trait_n){
  if(is.null(options$allow_spells)) return(0)
  if(!options$allow_spells) return(0)
  
  spell_list = g_spells(build, options, class_n, feat_list)
  spell_list_id = paste(names(spell_list), spell_list)
  uniq_spell_list = spell_list[!duplicated(spell_list_id)]
  
  trait = 0
  for(spell_n in uniq_spell_list){
    spell_id = paste(names(spell_n), spell_n)
    metamagic = names(spell_id)
    if(is.null(metamagic)) metamagic = ""
    if(is.null(spells_av[[spell_n]])) next
    if(is.null(spells_av[[spell_n]][[trait_n]])) next
    
    number_of_times_cast = sum(spell_list_id == spell_id, na.rm=T)
    
    trait = trait + spells_av[[spell_n]][[trait_n]](build, options, feat_list, class_n, metamagic) * number_of_times_cast
  }
  
  return(trait)
}
feats_av[["Extend Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                             prereq_level=0,
                                             prereq_feats=c(),
                                             prereq_spellLevel=1)
feats_av[["Empower Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                  prereq_level=0,
                                  prereq_feats=c(),
                                  prereq_spellLevel=2)
feats_av[["Maximize Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                   prereq_level=0,
                                   prereq_feats=c(),
                                   prereq_spellLevel=3)
feats_av[["Quicken Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                    prereq_level=0,
                                    prereq_feats=c(),
                                    prereq_spellLevel=4)
feats_av[["Silent Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                   prereq_level=0,
                                   prereq_feats=c(),
                                   prereq_spellLevel=1)
feats_av[["Still Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                  prereq_level=0,
                                  prereq_feats=c(),
                                  prereq_spellLevel=1)

feats_av[["Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                             prereq_level=0,
                                             prereq_feats=c(),
                                             prereq_casterLevel=1,
                                             DC_bonus=list("Evocation"=1))

feats_av[["Greater Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                             prereq_level=0,
                                             prereq_feats=c("Spell Focus (Evocation)"),
                                             prereq_casterLevel=1,
                                             DC_bonus=list("Evocation"=1))

feats_av[["Epic Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                                     prereq_level=21,
                                                     prereq_feats=c("Greater Spell Focus (Evocation)"),
                                                     prereq_casterLevel=1,
                                                     DC_bonus=list("Evocation"=1))

feats_av[["Ranger Spellcasting"]] = list(feat_type=c(),
                                         damage=function(build, options, feat_list){
                                           SPELLCASTING_DAMAGE(build, options, feat_list, "Ranger")
                                         },
                                         enhancement_bonus=function(build, options, feat_list, stable_bonus){
                                           SPELLCASTING_ENHANCEMENT_BONUS(build, options, feat_list, "Ranger", stable_bonus)
                                         })

feats_av[["Wizard Spellcasting"]] = list(feat_type=c(),
                                         damage=function(build, options, feat_list){
                                           SPELLCASTING_DAMAGE(build, options, feat_list, "Wizard")
                                         },
                                         enhancement_bonus=function(build, options, feat_list, stable_bonus){
                                           SPELLCASTING_ENHANCEMENT_BONUS(build, options, feat_list, "Wizard", stable_bonus)
                                         },
                                         fixed_damage=function(build, options, feat_list){
                                           SPELLCASTING_FIXED_DAMAGE(build, options, feat_list, "Wizard")
                                         },
                                         hits_absorbed=function(build, options, feat_list){
                                           SPELLCASTING_MAX_DURATION_TRAIT(build, options, feat_list, "Wizard", "hits_absorbed")
                                         },
                                         HP=function(build, options, feat_list){
                                           SPELLCASTING_CUMULATIVE_TRAIT(build, options, feat_list, "Wizard", "HP")
                                         },
                                         concealment=function(build, options, feat_list){
                                           SPELLCASTING_MAX_DURATION_TRAIT(build, options, feat_list, "Wizard", "concealment")
                                         })