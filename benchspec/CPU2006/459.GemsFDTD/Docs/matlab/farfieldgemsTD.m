function [Einf_td,Ethf_td,Ephif_td,f_td,Etht_td,Ephit_td,Nfreq,ntstep,toffset,dt] = farfieldgemsTD(file)
% 
% [Einf_td,Ethf_td,Ephif_td,f_td,Etht_td,Ephit_td,Nfreq,ntstep,toffset,dt] = farfieldgemsTD(file)
% 
% This function reads the output file from NFT.f90 in the Frida code.
% 
% Einf_td     incident field in the freq. domain
% Ethf_td     theta component of far zone E field in the freq. domain
% Ephif_td    phi         -   "   -         -   "   -
% Etht_td     theta component of far zone E field in the time domain
% Ephit_td    phi         -   "   -         -   "   -                 
% f_td        frequency vector
% toffset     time offset
% dt          time step
% 
% Note that the fields from NFT are not normalized (with the
% incident field) unlike those from NFF.
% 
% Written by Åke Rydell 99-10-19.

if exist(file)~=2
  disp(['Cannot find filename ' file])
  % We need to assign all the output arguments
  Einf_td = 0;
  Ethf_td = 0;
  Ephif_td = 0;
  f_td = 0;
  Etht_td = 0;
  Ephit_td = 0;
  Nfreq = 0;
  ntstep = 0;
  toffset = 0;
  dt = 0;
  return
end

clear Ethf_td Ephif_td Einf_td Etht_td Ephit_td Nfreq ntstep toffset dt

fp = fopen(file,'r');

Nth    = fscanf(fp,'%d',1);
Nphi   = fscanf(fp,'%d',1);
Nfreq  = fscanf(fp,'%e',1);      % No. of freq. steps
ntstep = fscanf(fp,'%e\n',1);    % No. of time steps

th1     = fscanf(fp,'%e',1);
th2     = fscanf(fp,'%e',1);
phi1    = fscanf(fp,'%e',1);
phi2    = fscanf(fp,'%e',1);
toffset = fscanf(fp,'%e',1);     % Time offset
dt      = fscanf(fp,'%e\n',1);   % Time step

% Create angle vectors:
% th  = linspace(th1,th2,Nth);
% phi = linspace(phi1,phi2,Nphi);
Nangle = Nth*Nphi;

% Create frequency vector:
f_td = 0:1/(ntstep*dt):1/(2*dt);

Einf_td = fscanf(fp,'%e', [2,Nfreq]);
Einf_td = Einf_td(1,:) + i*Einf_td(2,:);
Einf_td = conj(Einf_td');

% Read E fields in the frequency domain:

Ef =  fscanf(fp,'%e', [4*Nfreq, Nangle]);
Ethf_td  = Ef(1:4:4*Nfreq,:) + i*Ef(2:4:4*Nfreq,:);
Ephif_td = Ef(3:4:4*Nfreq,:) + i*Ef(4:4:4*Nfreq,:);

% Read E fields in the time domain:

Et =  fscanf(fp,'%e', [2*ntstep, Nangle]);
Etht_td  = Et(1:2:2*ntstep,:);
Ephit_td = Et(2:2:2*ntstep,:);

fclose(fp);

return
