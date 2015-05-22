% 
% for h=1:14
%     h=num2str(h)
%     sferh=strcat('sfe',h)
%     befehl=strcat('sfmat=','csvread(''',sferh,'.csv'')')
%     eval(befehl)
%     [INCLUDEDR,PVALSR,EXCLUDEDR,INCLUDEDSQ,PVALSSQ,EXCLUDEDSQ] = mcs(sfmat,0.25,1000,12,'BLOCK')
%     befehl=strcat('csvwrite(''includeR',sferh,'.csv'',INCLUDEDR)')
%     eval(befehl)
%     befehl=strcat('csvwrite(''pvalsR',sferh,'.csv'',PVALSR)')
%     eval(befehl)
%     befehl=strcat('csvwrite(''excludeR',sferh,'.csv'',EXCLUDEDR)')
%     eval(befehl)
%     
%     befehl=strcat('csvwrite(''includeSQ',sferh,'.csv'',INCLUDEDSQ)')
%     eval(befehl)
%     befehl=strcat('csvwrite(''pvalsSQ',sferh,'.csv'',PVALSSQ)')
%     eval(befehl)
%     befehl=strcat('csvwrite(''excludeSQ',sferh,'.csv'',EXCLUDEDSQ)')
%     eval(befehl)
% end
 
for h=1:14
    h=num2str(h)
    sferh=strcat('sfe',h)
    befehl=strcat('sfmat=','csvread(''',sferh,'.csv'')')
    eval(befehl)
    [INCLUDEDR,PVALSR,EXCLUDEDR,INCLUDEDSQ,PVALSSQ,EXCLUDEDSQ] = mcs(sfmat,0.25,1000,12,'BLOCK')
    befehl=strcat('csvwrite(''includeR',sferh,'.csv'',INCLUDEDR)')
    eval(befehl)
    befehl=strcat('csvwrite(''pvalsR',sferh,'.csv'',PVALSR)')
    eval(befehl)
    befehl=strcat('csvwrite(''excludeR',sferh,'.csv'',EXCLUDEDR)')
    eval(befehl)
    
    befehl=strcat('csvwrite(''includeSQ',sferh,'.csv'',INCLUDEDSQ)')
    eval(befehl)
    befehl=strcat('csvwrite(''pvalsSQ',sferh,'.csv'',PVALSSQ)')
    eval(befehl)
    befehl=strcat('csvwrite(''excludeSQ',sferh,'.csv'',EXCLUDEDSQ)')
    eval(befehl)
end
    
    