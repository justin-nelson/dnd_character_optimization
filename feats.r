feats_av = list()
feats_av[["Template"]] = list(name="Template",
                              feat_type=c("Regular", "Fighter"),
                              prereq_level=31,
                              prereq_BAB=31,
                              prereq_stat=list("Strength"=100,
                                               "Dexterity"=100,
                                               "Constitution"=100,
                                               "Intelligence"=100,
                                               "Wisdom"=100,
                                               "Charisma"=100),
                              prereq_feats=c("Dodge"),
                              prereq_skills=list("any"=100),
                              prereq_class=list("Fighter"=100),
                              grant=c("Template 2"))

feats_av[["Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_stat=list("Dexterity"=15),
                                         num_attacks_off=function(build, burst){ return(1) })
feats_av[["Improved Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=6,
                                                  prereq_stat=list("Dexterity"=17),
                                                  prereq_feats=c("Two-Weapon Fighting"),
                                                  num_attacks_off=function(build, burst){ return(1) })
feats_av[["Greater Two-Weapon Fighting"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_level=0,
                                                 prereq_BAB=11,
                                                 prereq_stat=list("Dexterity"=19),
                                                 prereq_feats=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting"),
                                                 num_attacks_off=function(build, burst){ return(1) })
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
                                      attack_bonus=function(build, burst){ 
                                        if(hasAllFeats(build, "Deadly Defense")){ return(-3) }
                                        return(0)
                                      })
feats_av[["Whirlwind Attack"]] = list(feat_type=c("Regular", "Fighter"),
                                      prereq_stat=list("Dexterity"=13,
                                                       "Intelligence"=13),
                                      prereq_feats=c("Dodge", "Mobility", "Spring Attack", "Combat Expertise"),
                                      prereq_BAB=4)
feats_av[["Weapon Focus (Kukri)"]] = list(feat_type=c("Regular", "Fighter"),
                                          prereq_BAB=1,
                                          prereq_feats=c("Weapon Proficiency (Martial)"),
                                          grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)", "Weapon Focus (Invisible Blade)"),
                                          attack_bonus=function(build, burst){ 
                                            if(build$weapon == "Kukri") { return(1) } else { return(0) }
                                          })
feats_av[["Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                         prereq_BAB=1,
                                         prereq_feats=c("Weapon Proficiency (Monk)"),
                                         grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)"),
                                         attack_bonus=function(build, burst){ 
                                           if(build$weapon == "Kama") { return(1) } else { return(0) }
                                         })
feats_av[["Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                  prereq_BAB=4,
                                                  prereq_feats=c("Weapon Focus (Kama)"),
                                                  prereq_class=list("Fighter"=4),
                                                  damage=function(build, burst){return(1)})
feats_av[["Greater Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                 prereq_feats=c("Weapon Focus (Kama)"),
                                                 prereq_class=list("Fighter"=8),
                                                 attack_bonus=function(build, burst){ 
                                                   if(build$weapon == "Kama") { return(1) } else { return(0) }
                                                 })
feats_av[["Greater Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                          prereq_feats=c("Weapon Specialization (Kama)", "Greater Weapon Focus (Kama)"),
                                                          prereq_class=list("Fighter"=12),
                                                          damage=function(build, burst){return(1)})
feats_av[["Epic Weapon Focus (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                              prereq_level=21,
                                              prereq_feats=c("Greater Weapon Focus (Kama)"),
                                              attack_bonus=function(build, burst){ 
                                                if(build$weapon == "Kama") { return(2) } else { return(0) }
                                              })
feats_av[["Epic Weapon Specialization (Kama)"]] = list(feat_type=c("Regular", "Fighter"),
                                                       prereq_level=21,
                                                       prereq_feats=c("Epic Weapon Focus (Kama)", "Greater Weapon Specialization (Kama)"),
                                                       damage=function(build, burst){return(4)})
feats_av[["Melee Weapon Mastery (Slashing)"]] = list(feat_type=c("Fighter"),
                                                     prereq_class=list("Fighter"=12),
                                                     damage=function(build, burst){return(2)},
                                                     attack_bonus=function(build, burst){ 
                                                       if(build$weapon == "Kama") { return(2) } else { return(0) }
                                                     })
feats_av[["Weapon Finesse"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_BAB=1,
                                    attack_bonus=function(build, burst){ 
                                      if(build$stats[["Dexterity"]] > build$stats[["Strength"]]){
                                        return(g_stat_mod(build, "Dexterity") - g_stat_mod(build, "Strength"))
                                      }
                                      return(0)
                                    })
feats_av[["Epic Prowess"]] = list(feat_type=c("Regular", "Fighter"),
                                  prereq_level=21,
                                  attack_bonus=function(build, burst){ return(1) })
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
                                     damage=function(build, burst){
                                       if(!burst) { return(0) }
                                       if(hasAllFeats(build, "Dire Charge")){ return(g_aveDamageDice(2, 6)) }
                                       return(0)
                                     })
feats_av[["Dire Charge"]] = list(feat_type=c("Regular"),
                                 prereq_level=21,
                                 prereq_feats=c("Improved Initiative"),
                                 attack_bonus=function(build, burst){ 
                                   if(!burst) { return(0) } else { return(4) } 
                                 })
feats_av[["Deadly Defense"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_feats=c("Combat Expertise"),
                                    damage=function(build, burst){return(g_aveDamageDice(1,4))})
feats_av[["Combat Insight"]] = list(feat_type=c("Regular", "Fighter"),
                                    prereq_level=21,
                                    prereq_feats=c("Combat Expertise", "Epic Prowess"),
                                    damage=function(build, burst){
                                      if(build$stats[["Intelligence"]] > build$stats[["Strength"]]){
                                        return(g_stat_mod(build, "Intelligence") - g_stat_mod(build, "Strength"))
                                      }
                                      return(0)
                                    })
feats_av[["Insightful Strike"]] = list(feat_type=c(),
                                       damage=function(build, burst){
                                         if(g_stat_mod(build, "Intelligence") > 0){
                                           return(g_stat_mod(build, "Intelligence"))
                                         }
                                         return(0)
                                       })

feats_av[["Dervish Dance"]] = list(feat_type=c(),
                                   damage=function(build, burst){ 
                                     if(!burst) return(0)
                                     return(floor((g_classLevel(build, "Dervish")+1)/2))
                                   },
                                   attack_bonus=function(build, burst){ 
                                     if(!burst) return(0)
                                     return(floor((g_classLevel(build, "Dervish")+1)/2))
                                   })

feats_av[["A Thousand Cuts"]] = list(feat_type=c(),
                                     damage=function(build, burst){ 
                                       if(!burst) return(0)
                                       return(g_aveDamageDice(2, 6))
                                     })

feats_av[["Bleeding Wound I"]] = list(feat_type=c(),
                                      postCrit_damage=function(build, burst){
                                        if(!burst) return(0)
                                        return(6)
                                      })
feats_av[["Bleeding Wound II"]] = list(feat_type=c(),
                                       postCrit_damage=function(build, burst){
                                         if(!burst) return(0)
                                         return(6)
                                       })
feats_av[["Bleeding Wound III"]] = list(feat_type=c(),
                                        postCrit_damage=function(build, burst){
                                          if(!burst) return(0)
                                          return(6)
                                        })