
for h=1:15
    h=num2str(h)
    sferh=strcat('sfer',h)
    befehl=strcat('sfmat=','csvread(''',sferh,'.csv'')')
    eval(befehl)
    [INCLUDEDR,PVALSR,EXCLUDEDR] = mcs(sfmat,0.25,5000,12)
    befehl=strcat('csvwrite(''includeR',sferh,'.csv'',INCLUDEDR)')
    eval(befehl)
    befehl=strcat('csvwrite(''pvalsR',sferh,'.csv'',PVALSR)')
    eval(befehl)
    befehl=strcat('csvwrite(''excludeR',sferh,'.csv'',EXCLUDEDR)')
    eval(befehl)
end
 
    
    