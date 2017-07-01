source("feats.r")
source("spells.r")
source("class.r") # This needs to load after feats.r and spells.r
source("armors.r")
source("evaluateBuild.r")
source("functions.r")

getAvailability = function(avail_classes, class){
  if(is.null(avail_classes[[class]])) { 
    return(0) 
  } else {
    if(is.na(avail_classes[[class]])){
      return(0)
    } else {
      return(avail_classes[[class]])
    }
  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SetupFeatsRetrieve
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ok this is silly but one of the most run functions is getting feats
# and because the feats are in vector form retrieving them is slow
# this sets up a way to quickly retrieve the feats.
setupFeatsRetrieve = function(class_av, max_level=30){
  class_list = names(class_av)
  for(class_n in class_list){
    if(is.null(class_av[[class_n]][["feats"]])) next
    class_av[[class_n]][["feats_at_lv"]] = list()
    #class_av[[class_n]][["feats_at_lv"]][[1]] = class_av[[class_n]][["feats"]][[1]]
    
    for(lv in 1:max_level){
      if(length(class_av[[class_n]][["feats"]]) < lv) {
        if(lv == 1) { class_av[[class_n]][["feats_at_lv"]][[lv]] = c() 
        } else { class_av[[class_n]][["feats_at_lv"]][[lv]] = class_av[[class_n]][["feats_at_lv"]][[lv-1]] }
      } else {
        if(lv == 1) {
          class_av[[class_n]][["feats_at_lv"]][[lv]] = class_av[[class_n]][["feats"]][[lv]]
        } else { class_av[[class_n]][["feats_at_lv"]][[lv]] = unique(c(class_av[[class_n]][["feats_at_lv"]][[lv-1]], class_av[[class_n]][["feats"]][[lv]])) }
      }
    }
  }
  return(class_av)
}
cls_av = setupFeatsRetrieve(cls_av, 30)
GLOBAL_fastFeatRetrieve=T

GLOBAL_grantingFeats = g_grantingFeatList(feats_av)

preferredClassesUnmet = function(build, preferred_classes){
  unmet_pref = c()
  for(class_n in names(preferred_classes)){
    if(g_classLevel(build, class_n) < preferred_classes[[class_n]]) unmet_pref = c(unmet_pref, class_n)
  }
  return(unmet_pref)
}

CLASS_EXPLORATION_DEPTH = 3
calculateQualifiedClasses = function(avail_classes, build, preferred_classes=list(), prestigeSearch="", max_level=g_level(build)+1, cooling=0.9, options=list()){
  qual_class = c()
  feat_list = g_feats(build)
  
  #%%%%
  # Get the caster levels so I don't have to later for each feat.
  #%%%%
  caster_progression = list()
  for(class_n in g_uniqClassList(build)){
    if(is.null(cls_av[[class_n]]$isCaster)) next
    if(!cls_av[[class_n]]$isCaster) next
    caster_progression[[class_n]] = g_casterProgression(build, options, class_n, feat_list)
  }
  
  available_classes = names(cls_av)[names(cls_av) %in% names(avail_classes > 0)]
  for(class_n in available_classes){
    if(g_classLevel(build, class_n) >= avail_classes[[class_n]]) next # Are we past the predefined level?
    if(!is.null(cls_av[[class_n]][["prereq_BAB"]])){ if(cls_av[[class_n]][["prereq_BAB"]] > g_BAB(build)) next } #BAB Check
    if(!is.null(cls_av[[class_n]][["prereq_feats"]])){ if(!hasAllFeats(build, cls_av[[class_n]][["prereq_feats"]])) next } # prereq Feat Check
    if(!is.null(cls_av[[class_n]][["prereq_skills"]])){ 
      if(!all(g_skillMaxes(build, names(cls_av[[class_n]][["prereq_skills"]])) > cls_av[[class_n]][["prereq_skills"]])) next
    }
    if( !is.null(cls_av[[class_n]]$prereq_arcane_spellLevel) ){
      prereq_spellLevel_met = F
      for(class_n2 in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n2]]$isArcaneCaster)) next
        if(!cls_av[[class_n2]]$isArcaneCaster) next
        if(length(g_spellslots(build, options, class_n2, feat_list, caster_progression[[class_n2]])[cls_av[[class_n]]$prereq_arcane_spellLevel + 1]) == 0) next
        prereq_spellLevel_met = T
      }
      
      if(!prereq_spellLevel_met) next
    }
    
    qual_class = c(class_n, qual_class)
  }
  
  class_table = sapply(3-table(build$class), max, 0)
  class_table = class_table[names(class_table) != "any"]
  
  ## If I already have 4 classes, I cannot take any more
  max_classes = 4
  if(prestigeSearch != "" & !(prestigeSearch %in% qual_class)) max_classes = 3 
  if(length(class_table) >= max_classes)               { qual_class = qual_class[qual_class %in% build$class] }
  ## If I am above level 20, the 3b20 rule will not allow me to take more
  if(g_level(build) >= 20){ qual_class = qual_class[qual_class %in% build$class] }
  
  ## 3b20 rule, if we are already about to run into it, I cannot select a new class.
  if(g_level(build) >= 15 & g_level(build) < 20){ 
    if(g_level(build) + sum(class_table) >= 20){ 
      qual_class = qual_class[qual_class %in% names(which(class_table > 0))]
    }
    if(g_level(build) + sum(class_table) + 3 >= 21){ 
      qual_class = qual_class[qual_class %in% build$class]
    }
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to prestigeClass
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(prestigeSearch != ""){
    if(any(qual_class == prestigeSearch, na.rm=T)) qual_class = c(prestigeSearch)
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to preferred classes
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 
  unmet_pref = preferredClassesUnmet(build, preferred_classes)
  if(any(qual_class %in% unmet_pref, na.rm=T)) qual_class = qual_class[qual_class %in% unmet_pref]
  
  if(length(qual_class) == 0) return(c("any"))
  return(qual_class)
}

getChoosableFeatList = function(feats_av){
  feat_list = c()
  for(feat_n in names(feats_av)){
    if(is.null(feats_av[[feat_n]][["feat_type"]])) next
    if(length(feats_av[[feat_n]][["feat_type"]]) == 0) next
    feat_list = c(feat_list, feat_n)
  }
  return(feat_list)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### PopulateFeats: return build
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sometimes I have a build that does not have feats
# This function populates the feat list, with greedy optimization
populateFeats_callCount = 0
populateFeats = function(avail_classes, build, cooling=0.9, options=list()){
  if(length(build$feats) > g_level(build)) return(build)
  populateFeats_callCount <<- populateFeats_callCount + 1
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(build, options)
  
  for(i in length(build$feats):g_level(build)){
    level_build = createPrefixBuild(build, g_level(build) - i, F)
    new_feats = calculateNumberOfNewFeats(level_build)
    last_class = lastCharacterClass(level_build)
    if(sum(unlist(new_feats)) > 0){ # There should be a feat at this level
      if(length(build$feats) < i){ # But if there isn't, we need to add some ... optimally
        feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
        level_build$feats[[i]] = rep(NA, 1)
        for(j in 1:length(feat_types)){
          level_build$feats[[i]][j] = options$ord_feat_list[which(options$ord_feat_list %in% calculateQualifiedFeats(level_build, feat_types[j], ""))[1]]
        }
        build$feats[[i]] = level_build$feats[[i]]
      }
    }
  }
  return(build)
}
# 
# createBranchBuild = function(avail_classes, branch_build, qualified_classes, max_level=g_level(build)+1, cooling=0.9, class_n="any"){
#   available_levels = getAvailability(avail_classes, class_n) - g_classLevel(branch_build, class_n) 
#   max_ind = min(g_level(branch_build) + available_levels, max_level)
#   branch_build$class[(g_level(branch_build)+1):max_ind] = class_n
#   
#   if(g_level(branch_build) < max_level){
#     branch_build = createBranchBuild(avail_classes, 
#                                      branch_build, 
#                                      qualified_classes[qualified_classes != class_n], 
#                                      max_level,
#                                      cooling,
#                                      class_n)
#   }
#   return(branch_build)
# }

lastCharacterClass = function(build){ 
  last_class_index = which(is.na(build$class))[1]-1
  if(is.na(last_class_index)) return(build$class[length(build$class)])
  return(build$class[which(is.na(build$class))[1]-1]) 
}

createStatIncreaseList = function(feats_av){
  feat_stat_increases_list = list("Strength"=c(),
                                  "Dexterity"=c(),
                                  "Constitution"=c(),
                                  "Intelligence"=c(),
                                  "Wisdom"=c(),
                                  "Charisma"=c())
  
  for(feat_n in names(feats_av)){
    for(stat_n in names(feat_stat_increases_list)){
      if(!is.null(feats_av[[feat_n]][[paste("plus", stat_n, sep="")]])) { feat_stat_increases_list[[stat_n]] = c(feat_stat_increases_list[[stat_n]], feat_n) }
    }
  }
  
  return(feat_stat_increases_list)
}

feats_to_increase_stats = createStatIncreaseList(feats_av)

setupFeatsInFeatTypes = function(feats_av){
  feat_types = list()
  for(feat_n in names(feats_av)){
    if(is.null(feats_av[[feat_n]]$feat_type)) next
    for(feat_type_n in feats_av[[feat_n]]$feat_type){
      if(is.null(feat_types[[feat_type_n]])) feat_types[[feat_type_n]] = vector()
      feat_types[[feat_type_n]] = c(feat_types[[feat_type_n]], feat_n)
    }
  }
  
  return(feat_types)
}
GLOBAL_feat_types = setupFeatsInFeatTypes(feats_av)

calculateQualifiedFeats = function(build, feat_type, prestigeSearch=""){
  qual_feats = c()
  
  available_feats = GLOBAL_feat_types[[feat_type]]
  #g_featsInFeatType(feat_type)
  available_feats = available_feats[!available_feats %in% g_feats(build)]
  feat_list = g_feats(build)
  
  #%%%%
  # Get the caster levels so I don't have to later for each feat.
  #%%%%
  caster_progression = list()
  for(class_n in g_uniqClassList(build)){
    if(is.null(cls_av[[class_n]]$isCaster)) next
    if(!cls_av[[class_n]]$isCaster) next
    caster_progression[[class_n]] = g_casterProgression(build, options, class_n, feat_list)
  }
  
  for(feat_n in available_feats){
    if( !is.null(feats_av[[feat_n]]$background) ) { if(feats_av[[feat_n]]$background & g_level(build) != 1) next }
    if( !is.null(feats_av[[feat_n]][["prereq_level"]])  ){ if(feats_av[[feat_n]][["prereq_level"]] > g_level(build)) next } # Level Check
    if( !is.null(feats_av[[feat_n]][["prereq_BAB"]])    ){ if(feats_av[[feat_n]][["prereq_BAB"]] > g_BAB(build)) next } # BAB Check
    if( !is.null(feats_av[[feat_n]][["prereq_stat"]])   ){
      if(any(g_stat_mod(build, names(feats_av[[feat_n]][["prereq_stat"]]), TRUE) < feats_av[[feat_n]][["prereq_stat"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_feats"]])  ){ if(!hasAllFeats(build, feats_av[[feat_n]][["prereq_feats"]])) next } # prereq Feat Check
    if( !is.null(feats_av[[feat_n]][["prereq_skills"]]) ){ 
      if(any(g_skillMaxes(build, names(feats_av[[feat_n]][["prereq_skills"]])) < feats_av[[feat_n]][["prereq_skills"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_class"]])  ){
      prereq_classes_met = T
      for(class_n in names(feats_av[[feat_n]][["prereq_class"]])){
        if(g_classLevel(build, class_n) < feats_av[[feat_n]][["prereq_class"]][[class_n]]) prereq_classes_met = F
      }
      if(!prereq_classes_met) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_casterLevel"]]) ){
      prereq_casterLevel_met = F
      for(class_n in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n]]$isCaster)) next
        if(!cls_av[[class_n]]$isCaster) next
        if(caster_progression[[class_n]] < feats_av[[feat_n]][["prereq_casterLevel"]]) next
        prereq_casterLevel_met = T
      }
      
      if(!prereq_casterLevel_met) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_spellLevel"]]) ){
      prereq_spellLevel_met = F
      for(class_n in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n]]$isCaster)) next
        if(!cls_av[[class_n]]$isCaster) next
        if(is.na(g_spellslots(build, options, class_n, feat_list, caster_progression[[class_n]])[feats_av[[feat_n]][["prereq_spellLevel"]]+1])) next
        prereq_spellLevel_met = T
      }
      
      if(!prereq_spellLevel_met) next
    }
    qual_feats = c(qual_feats, feat_n)
  }
  
  #qual_feats = qual_feats[!qual_feats %in% g_feats(build)]
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to feats which qualify me for prestigeSearch
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # This is useful for things such as prestige search.
  unmet_pref = g_prestigeFeatRequirements(build, prestigeSearch)
  if(any(qual_feats %in% unmet_pref, na.rm=T)) qual_feats = qual_feats[qual_feats %in% unmet_pref]
  
  if(length(qual_feats) == 0) return(c("any"))
  return(qual_feats)
}

assessFeats = function(build, feat_type, level, slot){
  build$feats[[level]][[slot]] = NA
  qual_feats = c()
  
  available_feats = GLOBAL_feat_types[[feat_type]]
  #g_featsInFeatType(feat_type)
  available_feats = available_feats[!available_feats %in% g_feats(build)]
  feat_list = g_feats(build)
  
  #%%%%
  # Get the caster levels so I don't have to later for each feat.
  #%%%%
  caster_progression = list()
  for(class_n in g_uniqClassList(build)){
    if(is.null(cls_av[[class_n]]$isCaster)) next
    if(!cls_av[[class_n]]$isCaster) next
    caster_progression[[class_n]] = g_casterProgression(build, options, class_n, feat_list)
  }
  
  for(feat_n in available_feats){
    if( !is.null(feats_av[[feat_n]][["prereq_level"]])  ){ if(feats_av[[feat_n]][["prereq_level"]] > g_level(build)) next } # Level Check
    if( !is.null(feats_av[[feat_n]][["prereq_BAB"]])    ){ if(feats_av[[feat_n]][["prereq_BAB"]] > g_BAB(build)) next } # BAB Check
    if( !is.null(feats_av[[feat_n]][["prereq_stat"]])   ){
      if(any(g_stat_mod(build, names(feats_av[[feat_n]][["prereq_stat"]]), TRUE) < feats_av[[feat_n]][["prereq_stat"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_feats"]])  ){ if(!hasAllFeats(build, feats_av[[feat_n]][["prereq_feats"]])) next } # prereq Feat Check
    if( !is.null(feats_av[[feat_n]][["prereq_skills"]]) ){ 
      if(any(g_skillMaxes(build, names(feats_av[[feat_n]][["prereq_skills"]])) < feats_av[[feat_n]][["prereq_skills"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_class"]])  ){
      prereq_classes_met = T
      for(class_n in names(feats_av[[feat_n]][["prereq_class"]])){
        if(g_classLevel(build, class_n) < feats_av[[feat_n]][["prereq_class"]][[class_n]]) prereq_classes_met = F
      }
      if(!prereq_classes_met) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_casterLevel"]]) ){
      prereq_casterLevel_met = F
      for(class_n in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n]]$isCaster)) next
        if(!cls_av[[class_n]]$isCaster) next
        if(caster_progression[[class_n]] < feats_av[[feat_n]][["prereq_casterLevel"]]) next
        prereq_casterLevel_met = T
      }
      
      if(!prereq_casterLevel_met) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_spellLevel"]]) ){
      prereq_spellLevel_met = F
      for(class_n in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n]]$isCaster)) next
        if(!cls_av[[class_n]]$isCaster) next
        if(is.na(g_spellslots(build, options, class_n, feat_list, caster_progression[[class_n]])[feats_av[[feat_n]][["prereq_spellLevel"]]+1])) next
        prereq_spellLevel_met = T
      }
      
      if(!prereq_spellLevel_met) next
    }
    qual_feats = c(qual_feats, feat_n)
  }
  
  scores = rep(0, length(qual_feats))
  names(scores) = qual_feats
  for(ind in 1:length(qual_feats)){
    feat_n = qual_feats[ind]
    test_build = build
    test_build$feats[[level]][slot] = qual_feats[ind]
    test_build = optimizeSpellList(test_build, options, TRUE)
    test_build = updateStats(test_build, options)
    test_build = updateSaves(test_build)
    test_build = updateSpellDCs(test_build, options)
    scores[ind] = evaluateBuild(test_build, options)$score
  }
  
  scores = scores[order(scores, decreasing=T)]
  
}

calculateNumberOfNewFeats = function(build){
  new_feats = list("Regular"=0)
  
  if(g_level(build) == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(g_level(build) == 1 & build$isHuman) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(g_level(build) <= 20 & g_level(build) %% 3 == 0) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(g_level(build) >  20 & g_level(build) %% 2 == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  
  last_class = lastCharacterClass(build)
  last_level = g_classLevel(build, last_class)
  
  new_feats[[last_class]] = 0
  if(!is.null(cls_av[[last_class]][["bonus_feats"]])){
    new_feats[[last_class]] = sum(cls_av[[last_class]][["bonus_feats"]] == last_level)
  }
  
  return(new_feats)
}

MONK_WEAPON_LIST = c("Kama", "Unarmed")

outputCharacter = function(build, options=list()){
  cat("\n\n")
  char_table = table(build$class)
  cat(paste(names(char_table), char_table,  sep=" ", collapse=" / "))
  cat("\n")
  cat(paste(1:30, ": ", build$class, sep="", collapse=", "))
  cat("\n")
  cat(paste(1:length(build$feats), ": ", build$feats, sep="", collapse="\n"))
  cat("\n")
  build.evaluation = evaluateBuild(build, options, g_level(build))
  cat("best_score: ")
  cat(build.evaluation$score, "\n")
  cat("armor_type: ")
  cat(build.evaluation$armorType)
  cat("\n\n")
  cat("Offensive Score|")
  cat(paste(1:30, ": ", format(build.evaluation$offensive_score, digits=3), sep="", collapse=", "))
  cat("\n\n")
  cat("Defensive Score|")
  cat(paste(1:30, ": ", format(build.evaluation$defensive_score, digits=3), sep="", collapse=", "))
  cat("\n\n")
  cat("Spells|\n")
  for(class_n in names(build$spell_lists)){
    for(i in 1:10){
      if(length(build$spell_lists[[class_n]]) < i) next
      cat(class_n, "|","spell level", i-1, "|", paste(names(build$spell_lists[[class_n]][[i]]), build$spell_lists[[class_n]][[i]], collapse=", ", sep=" "), "\n")
    }
  }
  #print(build$spell_lists)
}

levelClass = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=g_level(build)+1, cooling=0.9, options=list()){
  cur_lv = g_level(build)
  if(length(build$potential_classes) < cur_lv+1){
    build$potential_classes[[cur_lv+1]] = calculateQualifiedClasses(avail_classes, build, preferred_classes, prestigeSearch, max_level, cooling, options)
  }
  class_index = sample(1:length( build$potential_classes[[cur_lv+1]] ), 1)
  class_name = build$potential_classes[[cur_lv+1]][class_index]
  build$class[cur_lv+1] = class_name
  build$potential_classes[[cur_lv+1]] = build$potential_classes[[cur_lv+1]][-1 * class_index]
  return(build)
}

selectFeat = function(build, number, feat_type="Regular", prestigeSearch=""){
  cur_lv = g_level(build)
  if(length(build$potential_feats) < cur_lv){
    build$potential_feats[[cur_lv]] = list()
  } else if(is.null(build$potential_feats[[cur_lv]])){
    build$potential_feats[[cur_lv]] = list()
  }
  if(length(build$potential_feats[[cur_lv]]) < number){
    build$potential_feats[[cur_lv]][[number]] = calculateQualifiedFeats(build, feat_type, prestigeSearch) 
  }
  if(number == 1) {
    build$feats[[cur_lv]] = rep(NA, 1)
    build$feat_types[[cur_lv]] = rep(NA, 1)
  }
  
  feat_ind = sample(1:length( build$potential_feats[[cur_lv]][[number]] ), 1)
  feat_name = build$potential_feats[[cur_lv]][[number]][feat_ind]
  build$feats[[cur_lv]][number] = feat_name
  build$potential_feats[[cur_lv]][[number]] = build$potential_feats[[cur_lv]][[number]][-1 * feat_ind]
  build$feat_types[[cur_lv]][[number]] = feat_type
  
  return(build)
}

levelFeat = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=g_level(build)+1, cooling=0.9){
  new_feats = calculateNumberOfNewFeats(build)
  last_class = lastCharacterClass(build)
  if(sum(unlist(new_feats)) > 0){
    feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
    for(i in 1:sum(unlist(new_feats))){
      build = selectFeat(build, i, feat_types[i], prestigeSearch)
    }
  } else {
    build$potential_feats[[g_level(build)]] = list()
  }
  return(build)
}

levelup = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=g_level(build)+1, cooling=0.9, options=list()){
  build = levelClass(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
  build = levelFeat(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling)
  return(build)
}

cullSpellList = function(build, options, feat_list=NULL){
  mm_str = g_metamagicString(build)
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  # For each class
  for(class_n in g_uniqClassList(build)){
    build$caster_progression[[class_n]] = g_casterProgression(build, options, class_n, feat_list)
    
    if( is.null(cls_av[[class_n]]$isCaster) ) next
    if( !cls_av[[class_n]]$isCaster ) next
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Spell Slots
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Get the number of spell slots that I have available at each spell level.
    # This doesn't take into account equipment yet.
    #
    spell_slots = g_spellslots(build, options, class_n, feat_list, build$caster_progression[[class_n]])
    if(class_n == "Wizard" & !is.null(build$forbidden_school)) spell_slots = spell_slots + 1 # Wizard Specialization
    # Finally, set the initial spell list, if it's a full optimization this is empty otherwise
    # It's where we left off.
    spell_list = build$mm_spell_lists[[mm_str]][[class_n]]
    
    if(length(spell_slots) == 0) next
    for(spell_level in 15:(length(spell_slots)+1)){ spell_list[spell_level] = NULL }
    for(spell_level in length(spell_slots):1){
      if( length(spell_list[[spell_level]]) > spell_slots[spell_level] ) spell_list[[spell_level]] = spell_list[[spell_level]][1:spell_slots[spell_level]] 
    }
    build$spell_lists[[class_n]] = spell_list
  }
  return(build)
}

skipper = 0
optimizeSpellList = function(build, options, fullOptimization=FALSE){
  mm_str = g_metamagicString(build)
  global_evaluations_5 <<- global_evaluations_5 + 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Spell list initialization
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # It's possible the prefix build doesn't have spell lists
  # This code ensures that it will have all the spell lists I need
  #
  # I also handle setting up the spell repetition penalty and feat_lists
  #
  if( is.null(build$spell_lists) ) build$spell_lists = list()
  if( is.null(build$mm_spell_lists) ) build$mm_spell_lists = list()
  if( is.null(build$mm_spell_lists[[mm_str]]) ) build$mm_spell_lists[[mm_str]] = list()
  if( is.null(build$caster_progression) ) build$caster_progression = list()
  
  spell_repetition_penalty = 0
  if(!is.null(options$spell_repetition_penalty)) spell_repetition_penalty = options$spell_repetition_penalty
  feat_list = g_feats(build)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # For each class
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Ok, so I need to go through all of the classes and see if they have spells
  # For most classes this will quickly exit, however for classes that have class$isCaster
  # set to true I need to come up with a spell list.
  #
  for(class_n in g_uniqClassList(build)){
    if( is.null(cls_av[[class_n]]$isCaster) ) next
    if( !cls_av[[class_n]]$isCaster ) next
    if( is.null(build$mm_spell_lists[[mm_str]][[class_n]]) ) build$mm_spell_lists[[mm_str]][[class_n]] = vector("list", 10)
    if( is.null(build$caster_progression[[class_n]]) ) build$caster_progression[[class_n]] = 0
    build$caster_progression[[class_n]] = g_casterProgression(build, options, class_n, feat_list)
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Spell Slots
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Get the number of spell slots that I have available at each spell level.
    # This doesn't take into account equipment yet.
    #
    spell_slots = g_spellslots(build, options, class_n, caster_progression=build$caster_progression[[class_n]])
    if(class_n == "Wizard" & !is.null(build$forbidden_school)) spell_slots = spell_slots + 1 # Wizard Specialization
    # Finally, set the initial spell list, if it's a full optimization this is empty otherwise
    # It's where we left off.
    spell_list = build$mm_spell_lists[[mm_str]][[class_n]]
    if(fullOptimization) spell_list = vector("list", 10)
    best_spell_list = spell_list
    build$mm_spell_lists[[mm_str]][[class_n]] = best_spell_list
    
    # I need a base score for the build so I know the effect of the spell alone.
    base_build = build
    base_build$spell_lists[[class_n]] = best_spell_list
    best_score = evaluateBuild(base_build, options, levels_to_evaluate=1)$score
    
    if(length(spell_slots) == 0) next
    for(spell_level in 15:(length(spell_slots)+1)){ spell_list[[spell_level]] = NULL }
    for(spell_level in length(spell_slots):1){
      if( length(best_spell_list[[spell_level]]) > spell_slots[spell_level] ) best_spell_list[[spell_level]] = best_spell_list[[spell_level]][1:spell_slots[spell_level]] 
      if( is.null(cls_av[[class_n]]$spell_list) ) next
      if( is.null(cls_av[[class_n]]$spell_list[[spell_level]]) ) next
      if( length(cls_av[[class_n]]$spell_list[[spell_level]]) == 0 ) next
      if( spell_slots[spell_level] == 0 ) next
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Get the list of possible spells
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # The spell lists for the class include metamagic. This function gets the entire list
      # for this spell level and then filters out the ones with metamagic the build
      # doesn't have.
      #
      # I also take this opportunity to initialize spell_scores such that all
      # possible spells will be looked at, at least initially.
      # It might be possible to memorize spell scores to speed up the function
      #
      possible_spells = cls_av[[class_n]]$spell_list[[spell_level]]
      if(class_n == "Wizard" & !is.null(build$forbidden_school)) possible_spells = possible_spells[g_spellSchools(possible_spells) != build$forbidden_school]
      if(!is.null(names(possible_spells))){
        for(metamagic in names(possible_spells)){
          if(metamagic == "None") next
          if(!hasAllFeats(build, metamagic, feat_list)) possible_spells = possible_spells[names(possible_spells) != metamagic]
        }
      }
      possible_spells = sample(possible_spells) # Randomize the spell order
      spell_scores = rep(999999, length(possible_spells))
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Minimum slot
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # If I am not doing a full optimization, then I only care to optimize anything at all
      # iff there is a new spell slot that isn't filled with a spell
      #
      # This code ensures the only thing I'm optimizing is novel spell slots
      #
      min_slot_to_check = 1
      if(!fullOptimization){ min_slot_to_check = max(1, length(spell_list[[spell_level]])) }
      if(length(spell_list[[spell_level]]) == spell_slots[spell_level]) {
        skipper <<- skipper + 1
        next
      }
        
      for(sl_ind in min_slot_to_check:spell_slots[spell_level]){
        previous_score = best_score
        best_score = -9999
        spell_list = best_spell_list
        #cat(spell_level, ":", sl_ind, "|")
        
        ss_ord = order(spell_scores, decreasing=T)
        possible_spells = possible_spells[ss_ord]
        spell_scores = spell_scores[ss_ord]
        for(ps_ind in 1:length(possible_spells)){
          if(spell_scores[ps_ind] + spell_repetition_penalty * (spell_slots[spell_level] - ps_ind) < max(spell_scores)) break
          spell_n = possible_spells[ps_ind]
          
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          # This is a final speedup, if I don't think the score will change, 
          # and this spell is not better than the current best score, then
          # computing the score is useless so instead I skip it
          #
          # The speedup flag is manually set and can be globally unset.
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          speedup = F
          if(!is.null(spells_av[[spell_n]]$speedup)) speedup = spells_av[[spell_n]]$speedup
          if(spell_scores[ps_ind] < max(spell_scores) & speedup) next
          
          spell_n = possible_spells[ps_ind]
          spell_list[[spell_level]][sl_ind] = spell_n
          names(spell_list[[spell_level]])[sl_ind] = names(spell_n)
          
          global_evaluations_3 <<- global_evaluations_3 + 1
          build$spell_lists[[class_n]] = spell_list
          build$mm_spell_lists[[mm_str]][[class_n]] = spell_list
          
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          # Sometimes spells change things, I need to account for that here
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          test_build = build
          if(!is.null(spells_av[[spell_n]]$enhancement_bonus)) {
            test_build = updateStats(test_build, options)
            test_build = updateSaves(test_build)
            test_build = updateSpellDCs(test_build, options)
          }
          
          new_score = evaluateBuild(test_build, options, levels_to_evaluate=1)$score - spell_repetition_penalty * sum(spell_list[[spell_level]] == spell_n, na.rm=T)
          spell_scores[ps_ind] = new_score - previous_score
          #cat(names(spell_n), spell_n, ":", new_score - previous_score, "\n")
          if(new_score > best_score){
            best_spell_n = spell_n
            best_score = new_score
            best_spell_list = spell_list
          }
        }
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # Sometimes spells change things, I need to account for that here
        # Now that it's locked in that the best spell changes things, I need
        # to change them in the actual build.
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        if(!is.null(spells_av[[best_spell_n]]$enhancement_bonus)) {
          build = updateStats(build, options)
          build = updateSaves(build)
          build = updateSpellDCs(build, options)
        }
      }
    }
    build$spell_lists[[class_n]] = best_spell_list
    build$mm_spell_lists[[mm_str]][[class_n]] = best_spell_list
  }
  return(build)
}

delevel = function(build, avail_classes, cooling, options=list(), prestigeSearch=""){
  while(g_level(build) > 0){
    if(length(build$potential_feats) < g_level(build)){ # There was no feat at this level, defeat
      build$class[g_level(build)]             = NA
    } else {
      new_feats = calculateNumberOfNewFeats(build)
      last_class = lastCharacterClass(build)
      if(sum(unlist(new_feats)) > 0){
        for(i in sum(unlist(new_feats)):1){ # set empty potential feat lists to null
          if(length(build$potential_feats[[g_level(build)]]) != 0){
            if(length(build$potential_feats[[g_level(build)]][[i]]) == 0){ 
              build$potential_feats[[g_level(build)]][i] = NULL
            } else {
              break
            }
          }
        }
      }
      
      if(length(build$potential_feats[[g_level(build)]]) == 0){ # if there are no more feats for this level
        build$feats[[g_level(build)]]           = NULL
        build$potential_feats[[g_level(build)]] = NULL
        build$class[g_level(build)]             = NA
      } else {
        feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
        for(i in length(build$potential_feats[[g_level(build)]]):sum(unlist(new_feats))){ build = selectFeat(build, i, feat_types[i], prestigeSearch) } # Relevel
        return(build)
      }
    }
    
    # If the code made it this far, I need to figure out if there are more classes to take, if so level up and break
    if(length(build$potential_classes) >= g_level(build)+1){ # Well, there's something there
      if(length(build$potential_classes[[g_level(build)+1]]) >= 1){ # Is there a class to take
        build = levelup(build, avail_classes, prestigeSearch=prestigeSearch, cooling=cooling, options=options)
        return(build)
      } else { # No class to take, set potential classes to NULL and delevel again
        if(length(build$potential_classes) >= g_level(build)+1){
          build$potential_classes[[g_level(build)+1]] = NULL
        }
      }
    }
  }
  return(build)
}

STAT_NAMES = c("Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma")
updateStats = function(build, options){
  feat_list = g_feats(build)
  build$stats = g_stats(build, options, STAT_NAMES, feat_list, TRUE)
  build$rest_stats = g_stats(build, options, STAT_NAMES, feat_list, FALSE)
  return(build)
}

SPELLSCHOOL_NAMES = c("Abjuration", "Conjuration", "Divination", "Enchantment", "Evocation", "Illusion", "Necromancy", "Transmutation")
updateSpellDCs = function(build, options){
  for(class_n in g_uniqClassList(build)){
    if(is.null(cls_av[[class_n]]$isCaster)) next
    if(!cls_av[[class_n]]$isCaster) next
    build$spell_DCs[[class_n]] = g_baseSpellDC(build, options, class_n, SPELLSCHOOL_NAMES)
  }
  return(build)
}

updateSaves = function(build){
  build$saves = g_saves(build)
  return(build)
}

build_character = function(avail_classes, 
                           max_level = 30, 
                           level_warp = 1, 
                           options=list(), 
                           is.final=T, 
                           prefix_build = blank_prefix_build, 
                           monoclass_depth=5, 
                           cooling=1, 
                           report=T, 
                           preferred_classes=list(), 
                           prestigeSearch=""){
  if(is.null(options$min_cooling_level)) options$min_cooling_level = 0
  if(level_warp > max_level) level_warp = max_level
  builds_analyzed = 0
  
  best_build = blank_prefix_build
  best_score = -9999
  best_build_scores = rep(0, 30)
  
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(best_build, options)
  
  if(max_level > g_level(prefix_build) + level_warp){
    build = build_character(avail_classes, 
                            max_level=max_level-1, 
                            level_warp = level_warp, 
                            options = options, 
                            is.final=F, 
                            prefix_build=prefix_build, 
                            monoclass_depth = monoclass_depth, 
                            report=report, 
                            cooling=cooling,
                            preferred_classes=preferred_classes,
                            prestigeSearch=prestigeSearch)
  } else {
    build = prefix_build
  }
  if(max_level - level_warp < 0){ return(build) }
  
  build = updateStats(build, options)
  build = updateSaves(build)
  base_score = evaluateBuild(build, options, g_level(build), level_warp)$score
  scores_bl = rep(0, 30)
  best_scores_bl = scores_bl
  
  start = T
  max_level_reached = F
  while(length(build$potential_classes) != 0 || start){
    start = F
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Level up the character to 30 so I can properly assess damage
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    while(g_level(build) < max_level){
      
      current_score = 0
      break_score = 0
      current_score = evaluateBuild(build, options, g_level(build), level_warp)$score
      if(g_level(build) > 0) scores_bl[g_level(build)] = current_score
      if( max_level_reached & g_level(build) > 0) {
        global_evaluations_1 <<- global_evaluations_1 + 1
        
        break_score = best_scores_bl[g_level(build)]
        #evaluateBuild(best_build, options, g_level(build), level_warp)$score - base_score
        #current_score = evaluateBuild(build, options, g_level(build), level_warp)$score
        if(break_score < 0) { break_score = break_score/cooling } else { break_score = break_score * cooling }
        
        if(g_level(build) >= options$min_cooling_level & (current_score - base_score) < break_score ) { break; }
      }
      # global_build <<- build
      # new_feats = paste(g_feats(build), collapse=", ")
      # #if(g_level(build) > 0) new_feats = paste(g_feats(build), collapse=", ")
      # write(paste(g_level(build), "| current score", current_score - base_score, "| break score", break_score, "| build:", build$class[!is.na(build$class)], "| ", new_feats,"\n"),
      #       file="builder.txt", append=T)
      
      build = levelup(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
      build = optimizeSpellList(build, options, FALSE)
      build = updateStats(build, options)
      build = updateSaves(build)
      build = updateSpellDCs(build, options)
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Assess
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if(g_level(build) == max_level){
      
      max_level_reached = T
      builds_analyzed = builds_analyzed + 1
      if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
      global_evaluations_2 <<- global_evaluations_2 + 1
      new_score = evaluateBuild(build, options, g_level(build), level_warp+1)$score
      
      if(new_score > best_score){
        best_build = build
        best_score = new_score
        best_scores_bl = scores_bl
        if(report) cat(max_level, "score:", best_score, "\n")
      }
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Delevel
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    build = delevel(build, avail_classes, cooling, options, prestigeSearch)
    build = cullSpellList(build, options)
    build = updateStats(build, options)
    build = updateSaves(build)
    build = updateSpellDCs(build, options)
  }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Monoclass Search
  #
  ### This is the second chain of the search. Instead of using a greedy approach to restrict the search space
  ### I am restricting the search space based on only looking at a single class. This should allow for a much
  ### deeper search. This is a good search to do because many class features require many class levels
  ### in order to function.
  #
  ### It is possible that the best build comes from this "deep" search instead of the shallow level warp search.
  ### Note, I will still level warp afterwards, however the next shallow search should easily find the new
  ### local maxima.
  #
  ### Many prestige classes have requirements which are not good however the class
  ### positively contributes to the evaluation function. Local optimization will never allow
  ### the prereqs to be taken so I need to do a "dive" for the prestige class.
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  best_score = evaluateBuild(best_build, options, max_level, monoclass_depth+1)$score
  base_class_build = createPrefixBuild(best_build, monoclass_depth, F)
  if(1 <= monoclass_depth 
     & length(g_uniqClassList(base_class_build)) < 4 
     & g_level(base_class_build) <= 20 
     & max_level > level_warp+1){
    
    if(report) cat("monoclass score:", best_score, "\n")
    for(class_n in names(which(avail_classes > 0))){
      # Also make sure I have levels of the class left. If I don't have them here then there is no use going back.
      if(g_classLevel(best_build, class_n) >= getAvailability(avail_classes, class_n)) next
      if(class_n == "any") next
      depth_to_search = max(min(monoclass_depth, g_level(best_build), max_level), 1)
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Start descending
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      for(i in 1:depth_to_search){
        class_build = createPrefixBuild(best_build, i, F)
        if(g_classLevel(class_build, class_n) == 0 & g_level(class_build) >= 19) next
        if(all(g_classList(best_build)[g_level(class_build):g_level(best_build)] == class_n)) next
        class_build = build_character(avail_classes,
                                      max_level,
                                      0,
                                      options,
                                      prefix_build = class_build,
                                      monoclass_depth = 0,
                                      is.final=F,
                                      report=F,
                                      cooling=cooling,
                                      preferred_classes=preferred_classes,
                                      prestigeSearch=class_n)
        if(g_classLevel(class_build, class_n) == 0) next
        builds_analyzed = builds_analyzed + 1
        new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1)$score
        #cat(class_n, "|", i, "|", new_score, "|", class_build$class, "\n")
        if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
        if(new_score > best_score){
          cat("Mono search found a new build!", "New Score:", new_score, "\n")
          best_build = class_build
          best_score = new_score
          cat(class_n, "|", i, "|", new_score, "|", class_build$class[!is.na(class_build$class)], "\n")
          # class_build = createPrefixBuild(best_build, i, F)
          # class_build = build_character(avail_classes,
          #                               max_level,
          #                               level_warp,
          #                               options,
          #                               prefix_build = class_build,
          #                               monoclass_depth = 0,
          #                               report=F,
          #                               cooling=cooling,
          #                               preferred_classes=preferred_classes,
          #                               prestigeSearch=class_n)
          # new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1)$score
          # 
          # if(new_score > best_score){ # Still the best build?
          #   global_best_score <<- best_score
          #   global_new_score <<- new_score
          #   cat("Mono search found a complex new build!", "New Score:", new_score, "\n")
          #   best_build = class_build
          #   best_score = new_score
          #   
          # }
          #if(report) cat("mono score:", best_score, "\n")
        }
      }
    }
  }
  
  if(report) cat(g_level(best_build), ":", best_build$class[!is.na(best_build$class)], "\n")
  best_build = createPrefixBuild(best_build, level_warp, is.final)
  if(is.final) {
    best_build = optimizeSpellList(best_build, options, TRUE)
    best_build = updateStats(best_build, options)
    best_build = updateSaves(best_build)
    best_build = updateSpellDCs(best_build, options)
  }
  return(best_build)
}

createPrefixBuild = function(build, levels_to_lose, is.final=F){
  build$potential_classes = lapply(build$potential_classes, function(x) x = character(0))
  build$potential_feats   = lapply(build$potential_feats, function(x) x = list())
  
  if(levels_to_lose > g_level(build)){
    levels_to_lose = g_level(build)
  }
  
  if(g_level(build) >= levels_to_lose & !is.final & levels_to_lose != 0){
    blankRange = (g_level(build) - levels_to_lose + 1):30
    
    build$feats[blankRange] = NULL
    build$potential_feats[blankRange] = NULL
    build$potential_classes[blankRange] = NULL
    build$class[blankRange] = NA
  }
  
  #build = cullSpellList(build, options)
  
  return(build)
}

doOutput = F
profile = F
if(profile) Rprof("CO_profiling.out", line.profiling=T)

global_feat_selections = 0
global_class_selections = 0
global_evaluations = 0
global_evaluations_1 = 0
global_evaluations_2 = 0
global_evaluations_3 = 0
global_evaluations_4 = 0
global_evaluations_5 = 0
metamagic_searches = 0

blank_prefix_build = list(class = rep(NA, 30),
                          feats = list(),
                          feat_types = list(),
                          potential_classes = list(),
                          potential_feats = list(),
                          weapon = list(name="Kama",
                                        damage=g_aveDamageDice(1, 6),
                                        size="Light",
                                        hands="Two-Weapon",
                                        damage_type="Slashing",
                                        enhancement_bonus=4,
                                        attack_bonus=0),
                          armor = list(armor_bonus=3,
                                       deflection_bonus=3,
                                       natural_bonus=3,
                                       shield_bonus=0,
                                       dodge_bonus=3),
                          base_stats=c("Strength"=8,
                                       "Dexterity"=16,
                                       "Constitution"=10,
                                       "Intelligence"=20,
                                       "Wisdom"=10,
                                       "Charisma"=10),
                          equipment_stats=c("Strength"=0,
                                            "Dexterity"=0,
                                            "Constitution"=3,
                                            "Intelligence"=3,
                                            "Wisdom"=0,
                                            "Charisma"=0),
                          stat_increase="Intelligence",
                          favored_class="Fighter",
                          forbidden_school="Necromancy",
                          isHuman=F)
blank_prefix_build$stats = blank_prefix_build$base_stats

is.final = T
max_level = 30
level_warp = 3
monoclass_depth = 11
cooling = 0.80
options=list(allow_burst=T,
             allow_spells=T,
             allow_favoredEnemy=T,
             feat_multiplier=0.03, # How much score is 1 feat worth
             min_cooling_level=3,
             enemy_attack_adjustment = 0,
             enemy_damage_adjustment = 0,
             enemy_defense_adjustment = 0,
             number_of_enemies = 1.6,
             combat_ratio = 0.25,
             spell_repetition_penalty = 1) 
#avail_classes = list("Wizard"=30, "Monk"=0, "Dervish"=10, "Invisible Blade"=5, "Fighter"=0, "Swashbuckler"=0, "Tempest"=5, "Ranger"=0, "Whirling Dervish"=10)
avail_classes = list("Wizard"=30, "Arcane Scholar"=10)
windmaster_char = build_character(avail_classes,
                                  max_level = max_level,
                                  level_warp = level_warp,
                                  monoclass_depth = monoclass_depth,
                                  cooling = cooling,
                                  options = options)
windmaster_char = optimizeSpellList(windmaster_char, options, TRUE)
Rprof(NULL)
if(profile) summaryRprof("CO_profiling.out")
if(doOutput) outputCharacter(windmaster_char, options)



