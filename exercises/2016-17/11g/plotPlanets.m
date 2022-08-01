path = 'data';
filenames={'Mercury.txt','Venus.txt','Earth.txt','Mars.txt','Jupiter.txt','Saturn.txt','Uranus.txt','Neptune.txt','Pluto.txt'};
N = length(filenames);
data = cell(N,1);
formatSpec = '%f %f %f %f %f';
plot3(0,0,0,'+');
axis equal
hold on;
for i = 1:N
  filename = fullfile(path,filenames{i});
  disp(filename)
  fid = fopen(filename);
  thisline = '';
  while ~feof(fid) && ~strcmp(thisline,'$$SOE')
    thisline = fgetl(fid);
  end
  C = cell(0,5);
  thisline = '';
  while ~feof(fid) && ~strcmp(thisline,'$$EOE')
    thisline = fgetl(fid);
    C(end+1,:) = textscan(thisline,formatSpec,1);
  end
  %  C(end,:)=[];
  C = {cat(1,C{1:end-1,1}),cat(1,C{1:end-1,2}),cat(1,C{1:end-1,3}),cat(1,C{1:end-1,4}),cat(1,C{1:end-1,5})};

  x = C{4}.*sin(C{2}*pi/180).*cos(C{3}*pi/180);
  y = C{4}.*sin(C{2}*pi/180).*sin(C{3}*pi/180);
  z = C{4}.*cos(C{2}*pi/180);
  data{i} = [cat(1,C{1:end,1}),x,y,z];
  plot3(x,y,z,'.');
end
hold off

G = 6.67384e-11; % N (m / kg)^2, N = kg m / s^2
AUPerM = 149597870700; % m
Msun = 1.989e30; % kg
sPerDay = 24*60*60; % s
GM = G*Msun/AUPerM^3*sPerDay^2; % (kg m / s^2) (m / kg)^2 kg (AU^3 / m^3) (s^2 / day^2) = (AU^3 / day^2)
dt = 0.1;
t = 0;
r = cell(size(data,1),2);
v = cell(size(data,1),2);
h = cell(size(data,1),2);
for i = 1:size(data,1)
  p = data{i};
  v{i} = (p(2,2:end)-p(1,2:end))/(p(2,1)-p(1,1));
  r{i} = p(1,2:end);
  h{i} = [];
end
hold on;
while true;
  for i = 1:size(data,1)
    a = -GM/norm(r{i},2)^3 * r{i};
    t = t+dt;
    r{i} = r{i}+v{i}*dt;
    v{i} = v{i}+a*dt;
    delete(h{i})
    h{i} = plot3(r{i}(1),r{i}(2),r{i}(3),'ok');
  end
  drawnow;
end
hold off;
