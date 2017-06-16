spells_av = list()

spells_av[["Blades of Fire"]] = list(spell_level=1,
                                     time=function(build, options, feat_list, class_n){
                                       if(class_n == "Ranger") return(6*g_casterLevel(build, options, class_n, feat_list))
                                       return(12)
                                     },
                                     damage=function(build, options, feat_list, class_n){
                                       return(g_aveDamageDice(1, 8))
                                     })

spells_av[["Cat's Grace"]] = list(spell_level=2,
                                  time=function(build, options, feat_list, class_n){
                                    return(6*g_casterLevel(build, options, class_n, feat_list))
                                  },
                                  enhancement_bonus = function(build, options, feat_list, class_n){
                                    enhancement_bonus = c("Strength"=0,
                                                          "Dexterity"=4,
                                                          "Constitution"=0,
                                                             "Intelligence"=0,
                                                             "Wisdom"=0,
                                                             "Charisma"=0)
                                    return(enhancement_bonus)
                                  })
spells_av[["Animalistic Power"]] = list(spell_level=2,
                                  time=function(build, options, feat_list, class_n){
                                    return(6*g_casterLevel(build, options, class_n, feat_list))
                                  },
                                  enhancement_bonus = function(build, options, feat_list, class_n){
                                    enhancement_bonus = c("Strength"=2,
                                                          "Dexterity"=2,
                                                          "Constitution"=2,
                                                          "Intelligence"=0,
                                                          "Wisdom"=0,
                                                          "Charisma"=0)
                                    return(enhancement_bonus)
                                  })