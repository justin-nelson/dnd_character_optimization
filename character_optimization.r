source("feats.r")
source("class.r")
source("spells.r")
source("armors.r")
source("evaluateBuild.r")
source("functions.r")

calculateBAB = function(build){
  BAB = 0
  if(calculateLevel(build) == 0) return(0)
  for(class_n in g_uniqClassList(build)){
    classLevel = g_classLevel(build, class_n)
    if(class_n == "any") next
    if(cls_av[[class_n]][["BAB"]] == "high"){ BAB = BAB + classLevel }
    if(cls_av[[class_n]][["BAB"]] == "medium"){ BAB = BAB + floor(0.75*classLevel) }
    if(cls_av[[class_n]][["BAB"]] == "low"){ BAB = BAB + floor(0.5*classLevel) }
  }
  
  return(BAB)
}

calculateLevel = function(build){ 
  if(is.null(build$class)) return(0)
  return(length(g_classList(build)))
}

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

hasAllFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(all(feat_n %in% feat_list)) 
}

hasAnyFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(any(feat_n %in% feat_list)) 
}

preferredClassesUnmet = function(build, preferred_classes){
  unmet_pref = c()
  for(class_n in names(preferred_classes)){
    if(g_classLevel(build, class_n) < preferred_classes[[class_n]]) unmet_pref = c(unmet_pref, class_n)
  }
  return(unmet_pref)
}

CLASS_EXPLORATION_DEPTH = 3
calculateQualifiedClasses = function(avail_classes, build, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  qual_class = c()
  c_feats = g_feats(build)
  
  available_classes = names(cls_av)[names(cls_av) %in% names(avail_classes > 0)]
  for(class_n in available_classes){
    if(g_classLevel(build, class_n) >= avail_classes[[class_n]]) next # Are we past the predefined level?
    if(!is.null(cls_av[[class_n]][["prereq_BAB"]])){ if(cls_av[[class_n]][["prereq_BAB"]] > calculateBAB(build)) next } #BAB Check
    if(!is.null(cls_av[[class_n]][["prereq_feats"]])){ if(!hasAllFeats(build, cls_av[[class_n]][["prereq_feats"]])) next } # prereq Feat Check
    if(!is.null(cls_av[[class_n]][["prereq_skills"]])){ 
      if(!all(g_skillMaxes(build, names(cls_av[[class_n]][["prereq_skills"]])) > cls_av[[class_n]][["prereq_skills"]])) next
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
  if(calculateLevel(build) >= 20){ qual_class = qual_class[qual_class %in% build$class] }
  
  ## 3b20 rule, if we are already about to run into it, I cannot select a new class.
  if(calculateLevel(build) >= 15 & calculateLevel(build) < 20){ 
    if(calculateLevel(build) + sum(class_table) >= 20){ 
      qual_class = qual_class[qual_class %in% names(which(class_table > 0))]
    }
    if(calculateLevel(build) + sum(class_table) + 3 >= 21){ 
      qual_class = qual_class[qual_class %in% build$class]
    }
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to prestigeClass
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(prestigeSearch != ""){
    if(any(qual_class == prestigeSearch, na.rm=T)) qual_class = c(prestigeSearch)
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ### Prioritize Skills
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # It is possible for my prestigeSearch to go dry due to missing a skill
    # If that missing skill belongs to a class that is generally bad, I might skip a really good
    # prestige class for a long time while I wait for that skill
    # It might be worthwhile to pick up a "bad" class in order to qualify for a prestige class earlier
    # 
    # Only start down this path if I have enough class slots left
    # if(length(class_table) <= 2){
    #   skills = g_prestigeSkillRequirements(build, prestigeSearch)
    #   prioritized_classes = c()
    #   for(skill_n in names(skills)){
    #     if(skills[skill_n] < g_skillMaxes(build, skill_n) & !g_hasClassSkills(build, skill_n)){
    #       prioritized_classes = c(prioritized_classes, g_classesWithSkill(skill_n))
    #     }
    #   }
    #   if(any(qual_class %in% prioritized_classes, na.rm=T)) qual_class = qual_class[qual_class %in% prioritized_classes]
    # }
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to preferred classes
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # 
  unmet_pref = preferredClassesUnmet(build, preferred_classes)
  if(any(qual_class %in% unmet_pref, na.rm=T)) qual_class = qual_class[qual_class %in% unmet_pref]
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict the Branching Factor
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # classes are problematic in that they greatly add to the branching factor. In order to restrict this
  # I perform a branch cutting algorithm here.
  #qual_class = reduceClassBranchFactor(avail_classes, build, qual_class, min(calculateLevel(build)+CLASS_EXPLORATION_DEPTH, 30), cooling, options)
  #qual_class = reduceClassBranchFactor(avail_classes, build, qual_class, max_level, cooling, options, T)
  
  if(length(qual_class) == 0) return(c("any"))
  return(qual_class)
}

reduceClassBranchFactor = function(avail_classes, build, qualified_classes, max_level=calculateLevel(build)+1, cooling=0.9, options=list(), is.final=T, base_build=0){
  if(length(qualified_classes) == 0) { build$class[calculateLevel(build)+1:max_level] = "any" }
  if(calculateLevel(build) >= max_level) { return(qualified_classes) }  
  
  scores = list()
  for(class_n in qualified_classes){
    branch_build = createBranchBuild(avail_classes, build, qualified_classes, max_level, cooling, class_n)
    branch_build = populateFeats(avail_classes, branch_build, cooling, options)
    global_evaluations_3 <<- global_evaluations_3 + 1
    scores[class_n] = evaluateBuild(branch_build, options, calculateLevel(branch_build), max_level - calculateLevel(build) + 1, base_build)$score
  }
  reduced_list = names(which(scores >= cooling * max(unlist(scores, F, F))))
  return(reduced_list)
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
  if(length(build$feats) > calculateLevel(build)) return(build)
  populateFeats_callCount <<- populateFeats_callCount + 1
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(build, options)
  
  for(i in length(build$feats):calculateLevel(build)){
    level_build = createPrefixBuild(build, calculateLevel(build) - i, F)
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

resetBuild = function(){
  build = createPrefixBuild(windmaster_char, 15, F)
  return(build)
}

createBranchBuild = function(avail_classes, branch_build, qualified_classes, max_level=calculateLevel(build)+1, cooling=0.9, class_n="any"){
  available_levels = getAvailability(avail_classes, class_n) - g_classLevel(branch_build, class_n) 
  max_ind = min(calculateLevel(branch_build) + available_levels, max_level)
  branch_build$class[(calculateLevel(branch_build)+1):max_ind] = class_n
  
  if(calculateLevel(branch_build) < max_level){
    branch_build = createBranchBuild(avail_classes, 
                                     branch_build, 
                                     qualified_classes[qualified_classes != class_n], 
                                     max_level,
                                     cooling,
                                     class_n)
  }
  return(branch_build)
}

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

calculateQualifiedFeats = function(build, feat_type, prestigeSearch=""){
  qual_feats = c()
  
  available_feats = g_featsInFeatType(feat_type)
  available_feats = available_feats[!available_feats %in% g_feats(build)]
  feat_list = g_feats(build)
  for(feat_n in g_featsInFeatType(feat_type)){
    if( !is.null(feats_av[[feat_n]][["prereq_level"]])  ){ if(feats_av[[feat_n]][["prereq_level"]] > calculateLevel(build)) next } # Level Check
    if( !is.null(feats_av[[feat_n]][["prereq_BAB"]])    ){ if(feats_av[[feat_n]][["prereq_BAB"]] > calculateBAB(build)) next } # BAB Check
    if( !is.null(feats_av[[feat_n]][["prereq_stat"]])   ){
      if(any(g_stats(build, names(feats_av[[feat_n]][["prereq_stat"]])) < feats_av[[feat_n]][["prereq_stat"]])) next
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
        if(g_casterLevel(build, options, class_n, feat_list) < feats_av[[feat_n]][["prereq_casterLevel"]]) next
        prereq_casterLevel_met = T
      }
      
      if(!prereq_casterLevel_met) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_spellLevel"]]) ){
      prereq_spellLevel_met = F
      for(class_n in g_uniqClassList(build)){
        if(is.null(cls_av[[class_n]]$isCaster)) next
        if(!cls_av[[class_n]]$isCaster) next
        if(is.na(g_spellslots(build, options, class_n, feat_list)[feats_av[[feat_n]][["prereq_spellLevel"]]+1])) next
        prereq_spellLevel_met = T
      }
      
      if(!prereq_spellLevel_met) next
    }
    qual_feats = c(qual_feats, feat_n)
  }
  
  qual_feats = qual_feats[!qual_feats %in% g_feats(build)]
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Restrict to feats which qualify me for prestigeSearch
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # This is useful for things such as prestige search.
  unmet_pref = g_prestigeFeatRequirements(build, prestigeSearch)
  if(any(qual_feats %in% unmet_pref, na.rm=T)) qual_feats = qual_feats[qual_feats %in% unmet_pref]
  
  if(length(qual_feats) == 0) return(c("any"))
  return(qual_feats)
}

calculateNumberOfNewFeats = function(build){
  new_feats = list("Regular"=0)
  
  if(calculateLevel(build) == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) == 1 & build$isHuman) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) <= 20 & calculateLevel(build) %% 3 == 0) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) >  20 & calculateLevel(build) %% 2 == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  
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
  build.evaluation = evaluateBuild(build, options, calculateLevel(build))
  cat("best_score: ")
  cat(build.evaluation$score, "\n")
  cat("armor_type: ")
  cat(build.evaluation$armorType)
  cat("\n\n")
  cat("damage by level|")
  cat(paste(1:30, ": ", format(build.evaluation$damage, digits=3), sep="", collapse=", "))
  cat("\n\n")
  cat("Offensive Score|")
  cat(paste(1:30, ": ", format(build.evaluation$offensive_score, digits=3), sep="", collapse=", "))
  cat("\n\n")
  cat("Defensive Score|")
  cat(paste(1:30, ": ", format(build.evaluation$defensive_score, digits=3), sep="", collapse=", "))
  cat("\n\n")
}

levelClass = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  cur_lv = calculateLevel(build)
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
  cur_lv = calculateLevel(build)
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

levelFeat = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9){
  new_feats = calculateNumberOfNewFeats(build)
  last_class = lastCharacterClass(build)
  if(sum(unlist(new_feats)) > 0){
    feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
    for(i in 1:sum(unlist(new_feats))){
      build = selectFeat(build, i, feat_types[i], prestigeSearch)
    }
  } else {
    build$potential_feats[[calculateLevel(build)]] = list()
  }
  return(build)
}

levelup = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  build = levelClass(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
  build = levelFeat(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling)
  return(build)
}

delevel = function(build, avail_classes, cooling, options=list(), prestigeSearch=""){
  while(calculateLevel(build) > 0){
    if(length(build$potential_feats) < calculateLevel(build)){ # There was no feat at this level, defeat
      build$class[calculateLevel(build)]             = NA
    } else {
      new_feats = calculateNumberOfNewFeats(build)
      last_class = lastCharacterClass(build)
      if(sum(unlist(new_feats)) > 0){
        for(i in sum(unlist(new_feats)):1){ # set empty potential feat lists to null
          if(length(build$potential_feats[[calculateLevel(build)]]) != 0){
            if(length(build$potential_feats[[calculateLevel(build)]][[i]]) == 0){ 
              build$potential_feats[[calculateLevel(build)]][i] = NULL
            } else {
              break
            }
          }
        }
      }
      
      if(length(build$potential_feats[[calculateLevel(build)]]) == 0){ # if there are no more feats for this level
        build$feats[[calculateLevel(build)]]           = NULL
        build$potential_feats[[calculateLevel(build)]] = NULL
        build$class[calculateLevel(build)]             = NA
      } else {
        feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
        for(i in length(build$potential_feats[[calculateLevel(build)]]):sum(unlist(new_feats))){ build = selectFeat(build, i, feat_types[i], prestigeSearch) } # Relevel
        return(build)
      }
    }
    
    # If the code made it this far, I need to figure out if there are more classes to take, if so level up and break
    if(length(build$potential_classes) >= calculateLevel(build)+1){ # Well, there's something there
      if(length(build$potential_classes[[calculateLevel(build)+1]]) >= 1){ # Is there a class to take
        build = levelup(build, avail_classes, prestigeSearch=prestigeSearch, cooling=cooling, options=options)
        return(build)
      } else { # No class to take, set potential classes to NULL and delevel again
        if(length(build$potential_classes) >= calculateLevel(build)+1){
          build$potential_classes[[calculateLevel(build)+1]] = NULL
        }
      }
    }
  }
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
  
  builds_analyzed = 0
  
  best_build = blank_prefix_build
  best_score = -9999
  best_build_scores = rep(0, 30)
  
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(best_build, options)
  
  if(max_level > calculateLevel(prefix_build) + level_warp){
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
  if(max_level - level_warp <= 0){ return(build) }
  
  start = T
  max_level_reached = F
  while(length(build$potential_classes) != 0 || start){
    start = F
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Level up the character to 30 so I can properly assess damage
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    while(calculateLevel(build) < max_level){
      if( max_level_reached & calculateLevel(build) > 0) {
        global_evaluations_1 <<- global_evaluations_1 + 1

        
        eval_score = evaluateBuild(best_build, options, calculateLevel(build), level_warp)$score
        
        if(eval_score < 0) { eval_score = eval_score/cooling } else { eval_score = eval_score * cooling }
        
        if(calculateLevel(build) > options$min_cooling_level & evaluateBuild(build, options, calculateLevel(build), level_warp)$score < eval_score ) { break; }
      }
      build = levelup(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Assess
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if(calculateLevel(build) == max_level){
     
      max_level_reached = T
      builds_analyzed = builds_analyzed + 1
      if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
      global_evaluations_2 <<- global_evaluations_2 + 1
      new_score = evaluateBuild(build, options, calculateLevel(build), level_warp+1)$score
      
      if(new_score > best_score){
        best_build = build
        best_score = new_score
        if(report) cat(max_level, "score:", best_score, "\n")
      }
    }
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Delevel
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    build = delevel(build, avail_classes, cooling, options, prestigeSearch)
    
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
  base_class_build = createPrefixBuild(best_build, monoclass_depth, F)
  if(1 <= monoclass_depth 
     & length(g_uniqClassList(base_class_build)) < 4 
     & calculateLevel(base_class_build) <= 20 
     & max_level > level_warp+1){
    
    if(report) cat("monoclass score:", best_score, "\n")
    for(class_n in names(which(avail_classes > 0))){
      # Also make sure I have levels of the class left. If I don't have them here then there is no use going back.
      if(g_classLevel(best_build, class_n) >= getAvailability(avail_classes, class_n)) next
      if(class_n == "any") next
      depth_to_search = max(min(monoclass_depth, calculateLevel(best_build), max_level), 1)
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Start descending
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      for(i in 1:depth_to_search){
        class_build = createPrefixBuild(best_build, i, F)
        class_build = build_character(avail_classes,
                                      max_level,
                                      0,
                                      options,
                                      prefix_build = class_build,
                                      monoclass_depth = 0,
                                      report=F,
                                      cooling=cooling,
                                      preferred_classes=preferred_classes,
                                      prestigeSearch=class_n)
        builds_analyzed = builds_analyzed + 1
        new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1)$score
        if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
        if(new_score > best_score){
          print("Mono search found a new build!")

          class_build = createPrefixBuild(best_build, i, F)
          class_build = build_character(avail_classes,
                                        max_level,
                                        level_warp,
                                        options,
                                        prefix_build = class_build,
                                        monoclass_depth = 0,
                                        report=F,
                                        cooling=cooling,
                                        preferred_classes=preferred_classes,
                                        prestigeSearch=class_n)
          new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1)$score
          if(new_score > best_score){ # Still the best build?
            best_build = class_build
            best_score = new_score
            
          }
          if(report) cat("mono score:", best_score, "\n")
        }
      }
    }
  }
  
  if(report) cat(calculateLevel(best_build), ":", best_build$class[!is.na(best_build$class)], "\n")
  best_build = createPrefixBuild(best_build, level_warp, is.final)
  return(best_build)
}

createPrefixBuild = function(build, levels_to_lose, is.final=F){
  build$potential_classes = lapply(build$potential_classes, function(x) x = character(0))
  build$potential_feats   = lapply(build$potential_feats, function(x) x = list())
  
  if(levels_to_lose > calculateLevel(build)){
    levels_to_lose = calculateLevel(build)
  }
  
  if(calculateLevel(build) >= levels_to_lose & !is.final & levels_to_lose != 0){
    blankRange = (calculateLevel(build) - levels_to_lose + 1):30
    
    build$feats[blankRange] = NULL
    build$potential_feats[blankRange] = NULL
    build$potential_classes[blankRange] = NULL
    build$class[blankRange] = NA
  }
  
  return(build)
}

# Rprof("CO_profiling.out")

global_feat_selections = 0
global_class_selections = 0
global_evaluations = 0
global_evaluations_1 = 0
global_evaluations_2 = 0
global_evaluations_3 = 0
global_evaluations_4 = 0
global_evaluations_5 = 0

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
                          stats=c("Strength"=8,
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
                          spell_build="Area Blaster",
                          forbidden_school="Necromancy",
                          isHuman=F)

is.final = T
max_level = 30
level_warp = 2
monoclass_depth = 11
cooling = 0.90
options=list(allow_burst=T,
             allow_spells=T,
             allow_favoredEnemy=T,
             feat_multiplier=0.1, # How much score is 1 feat worth
             min_cooling_level=3,
             enemy_attack_adjustment = 0,
             enemy_damage_adjustment = 0,
             enemy_defense_adjustment = 0,
             number_of_enemies = 5,
             combat_ratio = 0.5) 
avail_classes = list("Wizard"=30, "Monk"=0, "Dervish"=10, "Invisible Blade"=5, "Fighter"=0, "Swashbuckler"=0, "Tempest"=5, "Ranger"=0, "Whirling Dervish"=10)
windmaster_char = build_character(avail_classes,
                                  max_level = max_level,
                                  level_warp = level_warp,
                                  monoclass_depth = monoclass_depth,
                                  cooling = cooling,
                                  options = options)

# Rprof(NULL)
# summaryRprof("CO_profiling.out")

outputCharacter(windmaster_char, options)

