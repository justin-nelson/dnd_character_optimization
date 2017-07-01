CHARGING_RATIO = 0.25
FIGHTING_RATIO = 0.75
FEINTING_RATIO = 0.80
REST_TIMER = 45

source("feat_statics.r")

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
feats_av[["Spellcasting Prodigy"]] = list(feat_type=c("Regular"),
                                          background=TRUE,
                                          DC_bonus=c("Abjuration"=1, 
                                                     "Conjuration"=1, 
                                                     "Divination"=1, 
                                                     "Enchantment"=1, 
                                                     "Evocation"=1, 
                                                     "Illusion"=1, 
                                                     "Necromancy"=1, 
                                                     "Transmutation"=1),
                                          inherentValue=1)
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
                                      if(g_stat_mod(build, "Dexterity") > g_stat_mod(build, "Strength")){
                                        return(g_stat_mod(build, "Dexterity") - g_stat_mod(build, "Strength"))
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
                                      if(g_stat_mod(build, "Intelligence") > g_stat_mod(build, "Strength")){
                                        return(g_stat_mod(build, "Intelligence") - g_stat_mod(build, "Strength"))
                                      }
                                      return(0)
                                    })
feats_av[["Insightful Strike"]] = list(feat_type=c(),
                                       damage=function(build, options, feat_list){
                                         if(g_stat_mod(build, "Intelligence") > 0){
                                           return(g_stat_mod(build, "Intelligence"))
                                         }
                                         return(0)
                                       })

feats_av[["Dervish Dance"]] = list(feat_type=c(),
                                   damage=DERVISH_DANCE,
                                   attack_bonus=DERVISH_DANCE)

feats_av[["A Thousand Cuts"]] = list(feat_type=c(),
                                     damage=function(build, options, feat_list){ 
                                       if(is.null(options$allow_burst)) return(0)
                                       if(!options$allow_burst) return(0)
                                       boost_ratio = min(1.5 / (g_level(build) * FIGHTING_RATIO), 1)
                                       return(boost_ratio * g_aveDamageDice(2, 6))
                                     })

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
                                          
                                          if(armorPass & weaponPass) return(max(g_stat_mod(build, "Intelligence"), g_classLevel(build, "Invisible Blade")))
                                          return(0)
                                        })
feats_av[["Weapon Focus (Slashing)"]] = list(feat_type=c(),
                                             prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Any)"]] = list(feat_type=c(),
                                        prereq_feats=c("Weapon Focus (Kukri)", "Weapon Focus (Kama)"))
feats_av[["Weapon Focus (Invisible Blade)"]] = list(feat_type=c(),
                                                    prereq_feats=c("Weapon Focus (Kukri)"))

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
                                         return(g_level(build))
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

feats_av[["Great Intelligence 1"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      plusIntelligence=1)
feats_av[["Great Intelligence 2"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Intelligence 1"),
                                      plusIntelligence=1)
feats_av[["Great Intelligence 3"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Intelligence 2"),
                                      plusIntelligence=1)
feats_av[["Great Intelligence 4"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Intelligence 3"),
                                      plusIntelligence=1)
feats_av[["Great Intelligence 5"]] = list(feat_type=c("Regular"),
                                      prereq_level=21,
                                      prereq_feats=c("Great Intelligence 4"),
                                      plusIntelligence=1)

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
                                     if(armorPass) return(g_stat_mod(build, "Wisdom"))
                                     return(0)
                                   })

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

feats_av[["Dervish AC Bonus I"]] = list(feat_type=c(),
                                        dodge_bonus=DERVISH_AC)
feats_av[["Dervish AC Bonus II"]] = list(feat_type=c(),
                                         dodge_bonus=DERVISH_AC)
feats_av[["Dervish AC Bonus III"]] = list(feat_type=c(),
                                          dodge_bonus=DERVISH_AC)

feats_av[["Extend Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                  isMetamagic=T,
                                  prereq_level=0,
                                  levelAdjust=1,
                                  prereq_feats=c(),
                                  prereq_spellLevel=1)
feats_av[["Empower Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                   isMetamagic=T,
                                   levelAdjust=2,
                                   prereq_level=0,
                                   prereq_feats=c(),
                                   prereq_spellLevel=2)
feats_av[["Maximize Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                    isMetamagic=T,
                                    levelAdjust=3,
                                    prereq_level=0,
                                    prereq_feats=c(),
                                    prereq_spellLevel=3)
feats_av[["Quicken Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                   isMetamagic=T,
                                   levelAdjust=4,
                                   prereq_level=0,
                                   prereq_feats=c(),
                                   prereq_spellLevel=4)
feats_av[["Silent Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                  isMetamagic=F,
                                  prereq_level=0,
                                  prereq_feats=c(),
                                  prereq_spellLevel=1)
feats_av[["Still Spell"]] = list(feat_type=c("Regular", "Wizard"),
                                 isMetamagic=F,
                                 prereq_level=0,
                                 prereq_feats=c(),
                                 prereq_spellLevel=1)
feats_av[["Improved Empower Spell"]] = list(feat_type=c(),
                                            isMetamagic=T,
                                            levelAdjust=1,
                                            prereq_level=0,
                                            prereq_feats=c(),
                                            prereq_spellLevel=2)
feats_av[["Improved Maximize Spell"]] = list(feat_type=c(),
                                             isMetamagic=T,
                                             levelAdjust=2,
                                             prereq_level=0,
                                             prereq_feats=c(),
                                             prereq_spellLevel=3)


feats_av[["Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                             prereq_level=0,
                                             prereq_feats=c(),
                                             prereq_casterLevel=1,
                                             DC_bonus=c("Evocation"=1))

feats_av[["Greater Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                                     prereq_level=0,
                                                     prereq_feats=c("Spell Focus (Evocation)"),
                                                     prereq_casterLevel=1,
                                                     DC_bonus=c("Evocation"=1))

feats_av[["Epic Spell Focus (Evocation)"]] = list(feat_type=c("Regular", "Wizard"),
                                                  prereq_level=21,
                                                  prereq_feats=c("Greater Spell Focus (Evocation)"),
                                                  prereq_casterLevel=1,
                                                  DC_bonus=c("Evocation"=1))

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

feats_av[["Skill Focus (Concentration)"]] = list(feat_type=c("Regular"),
                                                 prereq_BAB=0)
feats_av[["Skill Focus (Spellcraft)"]] = list(feat_type=c("Regular"),
                                              prereq_BAB=0)

feats_av[["Arcane Scholar Progression (Wizard)"]] = list(feat_type=c("Arcane Scholar"),
                                                         prereq_BAB=0,
                                                         class_to_progress="Wizard",
                                                         class_that_progresses="Arcane Scholar",
                                                         caster_progression=1:10)