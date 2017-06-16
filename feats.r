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

feats_av[["Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_stat=list("Dexterity"=15),
                                         num_attacks_off=function(build, options, feat_list){ 
                                           if(build$weapon$hands == "Two-Weapon") return(1)
                                           return(0)
                                         })
feats_av[["Improved Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=6,
                                                  prereq_stat=list("Dexterity"=17),
                                                  prereq_feats=c("Two-Weapon Fighting"),
                                                  num_attacks_off=function(build, options, feat_list){ 
                                                    if(build$weapon$hands == "Two-Weapon") return(1)
                                                    return(0)
                                                  })
feats_av[["Greater Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_level=0,
                                                 prereq_BAB=11,
                                                 prereq_stat=list("Dexterity"=19),
                                                 prereq_feats=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting"),
                                                 num_attacks_off=function(build, options, feat_list){ 
                                                   if(build$weapon$hands == "Two-Weapon") return(1)
                                                   return(0)
                                                 })
feats_av[["Perfect Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_level=21,
                                                 prereq_stat=list("Dexterity"=25),
                                                 prereq_feats=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting", "Greater Two-Weapon Fighting"))
feats_av[["Dodge"]] = list(feat_type=c("Regular", "Fighter"),
                           prereq_stat=list("Dexterity"=13))
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
                                        if(hasAllFeats(build, "Deadly Defense", feat_list)){ return(-3) }
                                        return(0)
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
feats_av[["Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_BAB=1,
                                         prereq_feats=c("Weapon Proficiency (Monk)"),
                                         grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)"),
                                         attack_bonus=function(build, options, feat_list){ 
                                           if(build$weapon$name == "Kama") { return(1) } else { return(0) }
                                         })
feats_av[["Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=4,
                                                  prereq_feats=c("Weapon Focus (Kama)"),
                                                  prereq_class=list("Fighter"=4),
                                                  damage=function(build, options, feat_list){ 
                                                    if(build$weapon$name == "Kama") { return(1) } else { return(0) }
                                                  })
feats_av[["Greater Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_feats=c("Weapon Focus (Kama)"),
                                                 prereq_class=list("Fighter"=8),
                                                 attack_bonus=function(build, options, feat_list){ 
                                                   if(build$weapon$name == "Kama") { return(1) } else { return(0) }
                                                 })
feats_av[["Greater Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                          prereq_feats=c("Weapon Specialization (Kama)", "Greater Weapon Focus (Kama)"),
                                                          prereq_class=list("Fighter"=12),
                                                          damage=function(build, options, feat_list){ 
                                                            if(build$weapon$name == "Kama") { return(1) } else { return(0) }
                                                          })
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
                                      if(build$weapon$size != "Light" | build$weapon$name != "Rapier") return(0)
                                      if(g_stat_mod(build, "Dexterity", feat_list, TRUE) > g_stat_mod(build, "Strength", feat_list, TRUE)){
                                        return(g_stat_mod(build, "Dexterity", feat_list, TRUE) - g_stat_mod(build, "Strength", feat_list, TRUE))
                                      }
                                      return(0)
                                    })
feats_av[["Epic Prowess"]] = list(feat_type=c("Regular", "Fighter"),
                                  prereq_level=21,
                                  attack_bonus=function(build, options, feat_list){ return(1) })
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

feats_av[["Dervish Dance"]] = list(feat_type=c(),
                                   damage=function(build, options, feat_list){ 
                                     if(is.null(options$allow_burst)) return(0)
                                     if(!options$allow_burst) return(0)
                                     bonus = floor((g_classLevel(build, "Dervish")+1)/2)
                                     boost_ratio = min((bonus * g_skillMaxes(build, "perform") / 20) / (calculateLevel(build) * FIGHTING_RATIO), 1)
                                     return(bonus * boost_ratio)
                                   },
                                   attack_bonus=function(build, options, feat_list){ 
                                     if(is.null(options$allow_burst)) return(0)
                                     if(!options$allow_burst) return(0)
                                     bonus = floor((g_classLevel(build, "Dervish")+1)/2)
                                     boost_ratio = min((bonus * g_skillMaxes(build, "Perform") / 20) / (calculateLevel(build) * FIGHTING_RATIO), 1)
                                     return(bonus * boost_ratio)
                                   })

feats_av[["A Thousand Cuts"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_burst)) return(0)
                                       if(!options$allow_burst) return(0)
                                       boost_ratio = min(1.5 / (calculateLevel(build) * FIGHTING_RATIO), 1)
                                       return(boost_ratio * g_aveDamageDice(2, 6))
                                     })

feats_av[["Bleeding Wound I"]] = list(feat_type=c(),
                                      postCrit_damage=function(build, options, feat_list){
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
                                      })
feats_av[["Bleeding Wound II"]] = list(feat_type=c(),
                                       postCrit_damage=function(build, options, feat_list){
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
                                       })
feats_av[["Bleeding Wound III"]] = list(feat_type=c(),
                                        postCrit_damage=function(build, options, feat_list){
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
                                        })
feats_av[["Weapon Focus (Slashing)"]] = list(feat_type=c(),
                                             prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Any)"]] = list(feat_type=c(),
                                        prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Invisible Blade)"]] = list(feat_type=c(),
                                                    prereq_feats=c("Weapon Focus (Kukri)"))

feats_av[["Tempest Defense I"]] = list(feat_type=c())
feats_av[["Tempest Defense II"]] = list(feat_type=c(),
                                        attack_bonus=function(build, options, feat_list){ 
                                          if(hasAllFeats(build, "Two-Weapon Fighting", feat_list) & build$weapon$hands == "Two-Weapon"){ return(1) }
                                          return(0)
                                        })
feats_av[["Tempest Defense III"]] = list(feat_type=c())
feats_av[["Tempest Defense IV"]] = list(feat_type=c())
feats_av[["Tempest Defense V"]] = list(feat_type=c(),
                                       attack_bonus=function(build, options, feat_list){ 
                                         if(hasAllFeats(build, "Two-Weapon Fighting", feat_list) & build$weapon$hands == "Two-Weapon"){ return(1) }
                                         return(0)
                                       })

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

feats_av[["Sneak Attack +1d6"]] = list(feat_type=c(),
                                        damage=function(build, options, feat_list){
                                          if(is.null(options$allow_burst)) return(0)
                                          if(!options$allow_burst) return(0)
                                          bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma", feat_list, TRUE)
                                          if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
                                          BLUFF_RATIO = max(min((bluff_skill - calculateLevel(build) + 10) / 20, 0.95), 0.05)
                                          SNEAK_RATIO = 0.2
                                          if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
                                          SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
                                          return(g_aveDamageDice(1,6) * SA_RATIO)
                                        })
feats_av[["Sneak Attack +2d6"]] = list(feat_type=c(),
                                       damage=function(build, options, feat_list){
                                         if(is.null(options$allow_burst)) return(0)
                                         if(!options$allow_burst) return(0)
                                         bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma", feat_list, TRUE)
                                         if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
                                         BLUFF_RATIO = max(min((bluff_skill - calculateLevel(build) + 10) / 20, 0.95), 0.05)
                                         SNEAK_RATIO = 0.2
                                         if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
                                         SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
                                         return(g_aveDamageDice(1,6) * SA_RATIO)
                                       })
feats_av[["Sneak Attack +3d6"]] = list(feat_type=c(),
                                       damage=function(build, options, feat_list){
                                         if(is.null(options$allow_burst)) return(0)
                                         if(!options$allow_burst) return(0)
                                         bluff_skill = g_skillMaxes(build, "Bluff") + g_stat_mod(build, "Charisma", feat_list, TRUE)
                                         if("Feint Mastery" %in% feat_list) bluff_skill = bluff_skill + 5
                                         BLUFF_RATIO = max(min((bluff_skill - calculateLevel(build) + 10) / 20, 0.95), 0.05)
                                         SNEAK_RATIO = 0.2
                                         if("Hide in Plain Sight" %in% feat_list){ SNEAK_RATIO = 1 }
                                         SA_RATIO = max(BLUFF_RATIO, SNEAK_RATIO)
                                         return(g_aveDamageDice(1,6) * SA_RATIO)
                                       })
feats_av[["Critical Sense 1"]] = list(feat_type=c(),
                                       attack_bonus=function(build, options, feat_list){ 
                                         return(1)
                                       })
feats_av[["Critical Sense 2"]] = list(feat_type=c(),
                                      attack_bonus=function(build, options, feat_list){ 
                                        return(1)
                                      })
feats_av[["Ranger Spellcasting"]] = list(feat_type=c(),
                                         damage=function(build, options, feat_list){
                                           if(is.null(options$allow_spells)) return(0)
                                           if(!options$allow_spells) return(0)
                                           
                                           spell_list = g_spells(build, options, "Ranger", feat_list)
                                           uniq_spell_list = unique(spell_list)
                                           damage = 0
                                           for(spell_n in uniq_spell_list){
                                             if(is.null(spells_av[[spell_n]])) next
                                             if(is.null(spells_av[[spell_n]]$damage)) next
                                             time = spells_av[[spell_n]]$time(build, options, feat_list, "Ranger") * sum(spell_list == spell_n, na.rm=T)
                                             DURATION_RATIO = min(time / (6* calculateLevel(build)), 1)
                                             damage = damage + spells_av[[spell_n]]$damage(build, options, feat_list, "Ranger") * DURATION_RATIO
                                           }
                                           
                                           return(damage)
                                         },
                                         enhancement_bonus=function(build, options, feat_list, stable_bonus){
                                           if(is.null(options$allow_spells)) return(0)
                                           if(!options$allow_spells) return(0)
                                           
                                           spell_list = g_spells(build, options, "Ranger", feat_list)
                                           uniq_spell_list = unique(spell_list)
                                           
                                           enhancement_bonus = stable_bonus
                                           
                                           for(spell_n in uniq_spell_list){
                                             if(is.null(spells_av[[spell_n]])) next
                                             if(is.null(spells_av[[spell_n]]$enhancement_bonus)) next
                                             
                                             time = spells_av[[spell_n]]$time(build, options, feat_list, "Ranger") * sum(spell_list == spell_n, na.rm=T)
                                             DURATION_RATIO = min(time / (6* calculateLevel(build)), 1)
                                             
                                             spell_EB = spells_av[[spell_n]]$enhancement_bonus(build, options, feat_list, "Ranger")
                                             spell_EB = enhancement_bonus + (enhancement_bonus - spell_EB) * DURATION_RATIO
                                             
                                             enhancement_bonus = pmax(enhancement_bonus, spell_EB)
                                           }

                                           return(round(enhancement_bonus))
                                         })
feats_av[["Favored Enemy I"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                        return(1)
                                      })
feats_av[["Favored Enemy II"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(1)
                                     })
feats_av[["Favored Enemy III"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(1)
                                     })
feats_av[["Favored Enemy IV"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(1)
                                     })
feats_av[["Favored Enemy V"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(1)
                                     })
feats_av[["Favored Enemy VI"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_favoredEnemy)) return(0)
                                       if(!options$allow_favoredEnemy) return(0)
                                       return(1)
                                     })
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