
for h=1:15
    h=num2str(h)
    sferh=strcat('sfer',h)
    befehl=strcat(sferh,'=','csvread(''',sferh,'.csv'')')
    eval(befehl)
end
 
    
    