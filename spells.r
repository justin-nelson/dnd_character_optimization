

BURSTATTACK = function(build, options, feat_list, class_n, spell_diceSides, spell_progression, max_dice, spell_n, metamagic, damagePlus=0, allowSave=T){
  caster_level = g_casterLevel(build, options, class_n, feat_list)
  base_damage = g_aveDamageDice(min(floor(caster_level * spell_progression), max_dice), spell_diceSides) + damagePlus
  if(metamagic == "Maximize Spell") base_damage = min(floor(caster_level * spell_progression), max_dice) * spell_diceSides
  if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
  
  # Burst Attack
  number_of_enemies = 1
  if(!is.null(options$number_of_enemies)) number_of_enemies = options$number_of_enemies
  
  # With Save
  if(allowSave){
    enemy_save = g_enemySave(build)
    spell_DC = g_spellDC(build, options, class_n, spell_n, feat_list)
    spellSuccessRate = g_spellSuccessRate(spell_DC, enemy_save)
  } else {
    spellSuccessRate = 1
  }
  
  damage = g_spellDamage_saveHalf(base_damage * number_of_enemies, spellSuccessRate)
  return(damage)
}

TIME_ROUNDPERLEVEL = function(build, options, feat_list, class_n, metamagic){
  seconds = 6*g_casterLevel(build, options, class_n, feat_list)
  if(metamagic == "Extend Spell") seconds = seconds * 2
  return(seconds)
}

spells_av = list()

spells_av[["Blades of Fire"]] = list(spell_level=1,
                                     time=function(build, options, feat_list, class_n, metamagic){
                                       seconds = 12
                                       if(class_n == "Ranger") seconds = 6*g_casterLevel(build, options, class_n, feat_list)
                                       if(metamagic == "Extend Spell") seconds = seconds * 2
                                       return(seconds)
                                     },
                                     damage=function(build, options, feat_list, class_n, metamagic){
                                       return(g_aveDamageDice(1, 8))
                                     })

spells_av[["Cat's Grace"]] = list(spell_level=2,
                                  time=TIME_ROUNDPERLEVEL,
                                  enhancement_bonus = function(build, options, feat_list, class_n, metamagic){
                                    enhancement_bonus = c("Strength"=0,
                                                          "Dexterity"=4,
                                                          "Constitution"=0,
                                                          "Intelligence"=0,
                                                          "Wisdom"=0,
                                                          "Charisma"=0)
                                    return(enhancement_bonus)
                                  })
spells_av[["Animalistic Power"]] = list(spell_level=2,
                                        time=TIME_ROUNDPERLEVEL,
                                        enhancement_bonus = function(build, options, feat_list, class_n, metamagic){
                                          enhancement_bonus = c("Strength"=2,
                                                                "Dexterity"=2,
                                                                "Constitution"=2,
                                                                "Intelligence"=0,
                                                                "Wisdom"=0,
                                                                "Charisma"=0)
                                          return(enhancement_bonus)
                                        })

spells_av[["Ray of Frost"]] = list(spell_level=0,
                                   spell_school="Conjuration",
                                   fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                     base_damage = g_aveDamageDice(1, 4) + 1
                                     if(metamagic == "Maximize Spell") base_damage = 5
                                     if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                     return(damage)
                                   })

spells_av[["Acid Splash"]] = list(spell_level=0,
                                  spell_school="Conjuration",
                                  fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                    number_of_enemies = 1
                                    if(!is.null(options$number_of_enemies)) number_of_enemies = options$number_of_enemies
                                    base_damage = g_aveDamageDice(1, 3)
                                    if(metamagic == "Maximize Spell") base_damage = 3
                                    if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                    
                                    return(number_of_enemies * base_damage)
                                  })
spells_av[["Magic Missile"]] = list(spell_level=1,
                                    spell_school="Evocation",
                                    fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                      caster_level = g_casterLevel(build, options, class_n, feat_list)
                                      number_of_missiles = min(ceiling(caster_level / 2), 9)
                                      base_damage = g_aveDamageDice(1, 4) + 1
                                      return(number_of_missiles * base_damage)
                                    })

spells_av[["Melf's Acid Arrow"]] = list(spell_level=2,
                                        spell_school="Conjuration",
                                        fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                          caster_level = g_casterLevel(build, options, class_n, feat_list)
                                          base_damage = g_aveDamageDice(3, 6)
                                          if(metamagic == "Maximize Spell") base_damage = 3 * 6
                                          if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                          duration = 1 + 1 * floor(caster_level / 3)
                                          if(metamagic == "Extend Spell") duration = duration * 2
                                          dot_damage = g_aveDamageDice(1,6)
                                          if(metamagic == "Maximize Spell") dot_damage = 6
                                          if(metamagic == "Empower Spell") dot_damage = dot_damage * 1.5
                                          
                                          return(base_damage + dot_damage * duration)
                                        })


spells_av[["Gedlee's Electric Loop"]] = list(spell_level=2,
                                             spell_school="Evocation",
                                             fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                               BURSTATTACK(build, options, feat_list, class_n, 6, 1/2, 12, "Gedlee's Electric Loop", metamagic)
                                             })

spells_av[["Fireball"]] = list(spell_level=3,
                               spell_school="Evocation",
                               fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                 BURSTATTACK(build, options, feat_list, class_n, 6, 1, 15, "Fireball", metamagic)
                               })

spells_av[["Ice Storm"]] = list(spell_level=4,
                                spell_school="Evocation",
                                fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                  BURSTATTACK(build, options, feat_list, class_n, 6, 1, 15, "Ice Storm", metamagic, g_aveDamageDice(5, 6), allowSave=FALSE)
                                })
spells_av[["Cone of Cold"]] = list(spell_level=5,
                                   spell_school="Evocation",
                                   fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                     BURSTATTACK(build, options, feat_list, class_n, 6, 1, 21, "Cone of Cold", metamagic)
                                   })
spells_av[["Chain Lightning"]] = list(spell_level=6,
                                      spell_school="Evocation",
                                      fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                        caster_level = g_casterLevel(build, options, class_n, feat_list)
                                        base_damage = g_aveDamageDice(min(caster_level, 24), 6)
                                        if(metamagic == "Maximize Spell") base_damage = min(caster_level, 24) * 6
                                        if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                        
                                        # Burst Attack
                                        number_of_enemies = 1
                                        if(!is.null(options$number_of_enemies)) number_of_enemies = options$number_of_enemies
                                        
                                        # With Save
                                        enemy_save = g_enemySave(build)
                                        spell_DC = g_spellDC(build, options, class_n, "Chain Lightning", feat_list)
                                        spellSuccessRate = g_spellSuccessRate(spell_DC, enemy_save)
                                        
                                        
                                        damage = g_spellDamage_saveHalf(base_damage, spellSuccessRate)
                                        return(damage + 0.5 * damage * (number_of_enemies - 1) )
                                      })

spells_av[["Flame Arrow"]] = list(spell_level=3,
                                  spell_school="Conjuration",
                                  fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                    caster_level = g_casterLevel(build, options, class_n, feat_list)
                                    enemy_save = g_enemySave(build)
                                    spell_DC = g_spellDC(build, options, class_n, "Flame Arrow", feat_list)
                                    spellSuccessRate = g_spellSuccessRate(spell_DC, enemy_save)
                                    
                                    base_damage = g_aveDamageDice(4, 6) + 1
                                    if(metamagic == "Maximize Spell") base_damage = 4 * 6 + 1
                                    if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                    arrows = floor(caster_level / 4)
                                    
                                    damage = g_spellDamage_saveHalf(base_damage * arrows, spellSuccessRate)
                                    return(damage)
                                  })

spells_av[["Isaac's Lesser Missile Storm"]] = list(spell_level=4,
                                                   spell_school="Evocation",
                                                   fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                                     caster_level = g_casterLevel(build, options, class_n, feat_list)
                                                     number_of_enemies = 1
                                                     if(!is.null(options$number_of_enemies)) number_of_enemies = options$number_of_enemies
                                                     base_damage = g_aveDamageDice(1, 6)
                                                     if(metamagic == "Maximize Spell") base_damage = 6
                                                     if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                                     missiles = min(caster_level, 10)
                                                     
                                                     return(missiles * base_damage)
                                                   })

spells_av[["Arc of Lightning"]] = list(spell_level=5,
                                       spell_school="Conjuration",
                                       fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                         BURSTATTACK(build, options, feat_list, class_n, 6, 1, 21, "Arc of Lightning", metamagic)
                                       })

spells_av[["Isaac's Greater Missile Storm"]] = list(spell_level=6,
                                                    spell_school="Evocation",
                                                    fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                                      caster_level = g_casterLevel(build, options, class_n, feat_list)
                                                      number_of_enemies = 1
                                                      if(!is.null(options$number_of_enemies)) number_of_enemies = options$number_of_enemies
                                                      base_damage = g_aveDamageDice(2, 6)
                                                      if(metamagic == "Maximize Spell") base_damage = 12
                                                      if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                                      missiles = min(10 * number_of_enemies, 20, caster_level)
                                                      
                                                      return(base_damage * missiles)
                                                    })

spells_av[["Delayed Blast Fireball"]] = list(spell_level=7,
                                             spell_school="Evocation",
                                             fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                               BURSTATTACK(build, options, feat_list, class_n, 6, 1, 27, "Delayed Blast Fireball", metamagic)
                                             })
spells_av[["Horrid Wilting"]] = list(spell_level=8,
                                     spell_school="Evocation",
                                     fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                       BURSTATTACK(build, options, feat_list, class_n, 6, 1, 27, "Horrid Wilting", metamagic)
                                     })

spells_av[["Polar Ray"]] = list(spell_level=8,
                                spell_school="Evocation",
                                fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                  caster_level = g_casterLevel(build, options, class_n, feat_list)
                                  base_damage = g_aveDamageDice(min(30, caster_level), 6)
                                  if(metamagic == "Maximize Spell") base_damage = min(30, caster_level) * 6
                                  if(metamagic == "Empower Spell") base_damage = base_damage * 1.5
                                  return(base_damage)
                                })

spells_av[["Burst of Glacial Wrath"]] = list(spell_level=9,
                                             spell_school="Evocation",
                                             fixed_damage=function(build, options, feat_list, class_n, metamagic){
                                               BURSTATTACK(build, options, feat_list, class_n, 6, 1, 30, "Burst of Glacial Wrath", metamagic, 10)
                                             })

spells_av[["Mirror Image"]] = list(spell_level=2,
                                   spell_school="Illusion",
                                   time=TIME_ROUNDPERLEVEL,
                                   hits_absorbed=function(build, options, feat_list, class_n, metamagic){
                                     caster_level = g_casterLevel(build, options, class_n, feat_list)
                                     hits_absorbed = min(g_aveDamageDice(1, 4) + floor(caster_level/3), 8)
                                     if(metamagic == "Maximize Spell") hits_absorbed = min(4 + floor(caster_level/3), 8)
                                     if(metamagic == "Empower Spell") hits_absorbed = hits_absorbed * 1.5
                                     return(hits_absorbed)
                                   })
spells_av[["Displacement"]] = list(spell_level=3,
                                   spell_school="Illusion",
                                   time=TIME_ROUNDPERLEVEL,
                                   concealment=function(build, options, feat_list, class_n, metamagic){ return(0.5) })