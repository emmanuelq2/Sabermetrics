retrosheet = read.csv('retrosheet 00-09.csv')
retrosheet$doubleheader = ifelse(retrosheet$game_number>0,1,0)
#model attendance with home team
attendanceModel = with(retrosheet,lm(attendance ~ home))
summary(attendanceModel)

#add visiting team
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor))
summary(attendanceModel)

#add day of week
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor  + day))
summary(attendanceModel)

#add day/night
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor  + day + daynight))
summary(attendanceModel)

#add total score
retrosheet$total_score = retrosheet$home_score + retrosheet$visitor_score
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor  + day + daynight + total_score))
summary(attendanceModel)

#add total home runs
retrosheet$total_hr = retrosheet$home_hr + retrosheet$visitor_hr
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor  + day + daynight + total_score + total_hr))
summary(attendanceModel)
#look, total score effect went away!

#add doubleheader indicator
attendanceModel = with(retrosheet,lm(attendance ~ home + visitor  + day + daynight + total_score + total_hr + doubleheader))
summary(attendanceModel)
