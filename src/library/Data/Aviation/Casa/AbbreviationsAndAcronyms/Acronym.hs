{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casa.AbbreviationsAndAcronyms.Acronym(
  Acronym(..)
, HasAcronym(..)
, acronymValues
, allAcronyms
) where

import Control.Applicative((<*>))
import Control.Category((.), id)
import Control.Lens(Traversal', Lens')
import Data.Eq(Eq)
import Data.Functor((<$>), fmap)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)
import Text.Fuzzy(Fuzzy(Fuzzy))
import Data.Monoid.Textual(TextualMonoid)

data Acronym =
  Acronym
    String -- acronym
    String -- meaning
    String -- source
  deriving (Eq, Ord, Show)

acronymValues ::
  Traversal'
    Acronym
    String
acronymValues f (Acronym acr mean src) =
  Acronym <$> f acr <*> f mean <*> f src

class HasAcronym c_a8nA where
  acronym :: Lens' c_a8nA Acronym
  meaning :: Lens' c_a8nA String
  {-# INLINE meaning #-}
  name :: Lens' c_a8nA String
  {-# INLINE name #-}
  source :: Lens' c_a8nA String
  {-# INLINE source #-}
  meaning = ((.) acronym) meaning
  name = ((.) acronym) name
  source = ((.) acronym) source
instance HasAcronym Acronym where
  {-# INLINE meaning #-}
  {-# INLINE name #-}
  {-# INLINE source #-}
  acronym = id
  meaning f_a8nB (Acronym x1_a8nC x2_a8nD x3_a8nE)
    = (fmap (\ y1_a8nF -> ((Acronym x1_a8nC) y1_a8nF) x3_a8nE))
        (f_a8nB x2_a8nD)
  name f_a8nG (Acronym x1_a8nH x2_a8nI x3_a8nJ)
    = (fmap (\ y1_a8nK -> ((Acronym y1_a8nK) x2_a8nI) x3_a8nJ))
        (f_a8nG x1_a8nH)
  source f_a8nL (Acronym x1_a8nM x2_a8nN x3_a8nO)
    = (fmap (\ y1_a8nP -> ((Acronym x1_a8nM) x2_a8nN) y1_a8nP))
        (f_a8nL x3_a8nO)

instance TextualMonoid s => HasAcronym (Fuzzy Acronym s) where
  acronym f (Fuzzy x r s) =
    fmap
      (\a -> Fuzzy a r s)
      (f x)

-- generated

allAcronyms ::
  [Acronym]
allAcronyms =
  [
    Acronym
      "...T"
      "Bearing (true)"
      "AIS"
  , Acronym
      "A"
      "Amber"
      "ICAO"
  , Acronym
      "A/A"
      "Air to air"
      "AIS"
  , Acronym
      "A/G"
      "Air-to-ground"
      "AIS"
  , Acronym
      "AAA"
      "Amended meterological message"
      "ICAO"
  , Acronym
      "AAAA"
      "Aerial Application Association of Australia"
      ""
  , Acronym
      "AAC"
      "Airworthiness Advisory Circular"
      "CAR 1988"
  , Acronym
      "AACC"
      "Area approach control centre"
      "AIS"
  , Acronym
      "AAD"
      "Assigned altitude deviation"
      "AIS"
  , Acronym
      "AAIM"
      "Aircraft Autonomous Integrity Monitor"
      ""
  , Acronym
      "AAIS"
      "Automatic aerodrome information service"
      "AIS"
  , Acronym
      "AAL"
      "Above aerodrome level"
      "AIS"
  , Acronym
      "AAR"
      "Authorised Airworthiness Representative"
      "CASR Part 183"
  , Acronym
      "AAT"
      "Administrative Appeals Tribunal"
      ""
  , Acronym
      "ABAA"
      "Amateur Built Aircraft Acceptance"
      "CASR Part 21"
  , Acronym
      "ABAS"
      "Aircraft Based Augmentation System"
      ""
  , Acronym
      "ABF"
      "Australian Ballooning Federation"
      ""
  , Acronym
      "ABI"
      "Advance boundary information"
      "AIS"
  , Acronym
      "ABM"
      "Abeam"
      "AIS"
  , Acronym
      "ABN"
      "Aerodrome beacon"
      "AIS"
  , Acronym
      "ABT"
      "About"
      "AIS"
  , Acronym
      "ABV"
      "Above..."
      "AIS"
  , Acronym
      "AC"
      "Advisory Circular"
      "CASR 1998"
  , Acronym
      "AC"
      "Altocumulus (met)"
      "ICAO/AIP"
  , Acronym
      "ACA"
      "Australian Commonwealth Authority"
      ""
  , Acronym
      "ACARS"
      "Aircraft communication addressing and reporting system (pronounced 'AY-CARS')"
      "AIS"
  , Acronym
      "ACAS"
      "Airborne Collision Avoidance System"
      "CAR 1988"
  , Acronym
      "ACC"
      "Area control centre"
      "AIS"
  , Acronym
      "ACCID"
      "Initial notification of an aircraft accident"
      "AIS"
  , Acronym
      "ACD"
      "Airways clearance delivery"
      "AIS"
  , Acronym
      "ACFT"
      "Aircraft"
      "AIS"
  , Acronym
      "ACJ-XXX"
      "Advisory Circular Joint (issued by the JAA, can be followed by a  country code)"
      "JAA"
  , Acronym
      "ACK"
      "Acknowledge"
      "AIS"
  , Acronym
      "ACL"
      "Altimeter check location"
      "ICAO"
  , Acronym
      "ACN"
      "Aircraft classification number"
      "ICAO"
  , Acronym
      "ACN"
      "Australian company number"
      ""
  , Acronym
      "ACPT"
      "Accept, accepted"
      "AIS"
  , Acronym
      "ACROB"
      "Acrobatics"
      ""
  , Acronym
      "Act"
      "the Civil Aviation Act 1988"
      ""
  , Acronym
      "ACT"
      "Active, activated, activity"
      "AIS"
  , Acronym
      "AD"
      "Aerodrome"
      "ICAO/AIP"
  , Acronym
      "AD"
      "Airworthiness Directive"
      "CASR Part 39"
  , Acronym
      "ADA"
      "Advisory area"
      "ICAO"
  , Acronym
      "ADAO"
      "Authorised Design Approval Organisation"
      "CASR Part 146"
  , Acronym
      "ADC"
      "Aerodrome chart"
      "AIS"
  , Acronym
      "ADDGM"
      "Aerodrome diagrams"
      "AIS"
  , Acronym
      "ADDN"
      "Addition, additional"
      "AIS"
  , Acronym
      "ADF"
      "Automatic Direction Finding (equipment)"
      "AIS"
  , Acronym
      "ADF"
      "Australian Defence Force"
      ""
  , Acronym
      "ADIZ"
      "Air Defence Identification Zone"
      "ICAO/AIS"
  , Acronym
      "ADJ"
      "Adjacent"
      "AIS"
  , Acronym
      "ADO"
      "Aerodrome office"
      "ICAO"
  , Acronym
      "ADQ"
      "Adequate aerodrome"
      "AIS"
  , Acronym
      "ADR"
      "Advisory route"
      ""
  , Acronym
      "ADS"
      "The address (when this abbreviation is used to request a repetition, the question (IMI) precedes the abbreviation, eg IMI ADS) (to be used in AFS as a procedure signal)"
      "ICAO/AIP"
  , Acronym
      "ADS"
      "Automatic Dependent Surveillance"
      "AIP/ICAO"
  , Acronym
      "ADS-B"
      "Automatic Dependent Surveillance - Broadcast"
      "AIP"
  , Acronym
      "ADS-C"
      "Automatic Dependent Surveillance - Contract"
      "AIP"
  , Acronym
      "ADSU"
      "Automatic Dependent Surveillance Unit"
      "ICAO"
  , Acronym
      "ADZ"
      "Advise"
      "AIS"
  , Acronym
      "AEB"
      "CASA's Airworthiness Engineering Branch"
      "CASA"
  , Acronym
      "AEP"
      "Aerodrome emergency plan"
      "AIS"
  , Acronym
      "AER"
      "Authorised Engineering Representative"
      "CASR Part 183"
  , Acronym
      "AERIS"
      "Automatic en route information service"
      "AIS"
  , Acronym
      "AFAP"
      "Australian Federation of Air Pilots"
      ""
  , Acronym
      "AFCS"
      "Automatic flight control system"
      "ICAO"
  , Acronym
      "AFIL"
      "Flight notification: - filed in the air, or - indicating the position at which ATS services will first be required"
      "AIS"
  , Acronym
      "AFIS"
      "Aerodrome flight information service"
      "ICAO/AIP"
  , Acronym
      "AFM"
      "Aircraft Flight Manual"
      "CAR 1988"
  , Acronym
      "AFM"
      "Yes, affirm, affirmative, that is correct"
      "AIS"
  , Acronym
      "AFRU"
      "Aerodrome Frequency Response Unit (also known as a &quot;beepback&quot; unit)"
      "AIS"
  , Acronym
      "AFS"
      "Aeronautical fixed service"
      "ICAO/AIP"
  , Acronym
      "AFT"
      "After"
      "AIS"
  , Acronym
      "AFTN"
      "Aeronautical fixed telecommunication network"
      "ICAO"
  , Acronym
      "AFZ"
      "Australian fishing zone(s)"
      "AIS"
  , Acronym
      "Ag"
      "Agriculture"
      ""
  , Acronym
      "AGA"
      "Again"
      "ICAO"
  , Acronym
      "AGD"
      "Attorney-General's Department"
      ""
  , Acronym
      "AGL "
      "Above Ground Level"
      "ICAO/AIP"
  , Acronym
      "AH"
      "Artificial Horizon"
      ""
  , Acronym
      "AH"
      "After hours"
      "AIS"
  , Acronym
      "AIC"
      "Aeronautical Information Circular"
      "AIP"
  , Acronym
      "AIDC"
      "Air traffic services inter-facitility data communications"
      "ICAO"
  , Acronym
      "AIP"
      "Aeronautical Information Publication"
      "ICAO/AIP"
  , Acronym
      "AIP ENR"
      "Aeronautical Information Publication En-route"
      ""
  , Acronym
      "AIP ERSA"
      "Aeronautical Information Publication En-route Supplement Australia"
      ""
  , Acronym
      "AIP GEN"
      "Aeronautical Information Publication General"
      ""
  , Acronym
      "AIRAC"
      "Aeronautical Information Regulation and Control"
      "ICAO/AIP"
  , Acronym
      "AIRC"
      "Aeronautical Information and Control"
      "ICAO"
  , Acronym
      "AIREP"
      "Air-Report"
      "AIP/ICAO"
  , Acronym
      "AIRMET"
      "Information in plain language concerning weather significant to light aircraft operations at or below 10,000FT"
      "AIS"
  , Acronym
      "AIS"
      "Aeronautical Information Service"
      "AIP"
  , Acronym
      "AL"
      "Approach Lights"
      "AIS"
  , Acronym
      "ALA"
      "Aircraft Landing Area "
      "AIP"
  , Acronym
      "ALA"
      "Alighting area"
      "ICAO"
  , Acronym
      "ALA/HLS"
      "Aircraft Landing Area/Helicopter Landing Site"
      ""
  , Acronym
      "ALAEA"
      "Australian Licensed Aircraft Engineers Association"
      ""
  , Acronym
      "ALARP"
      "As Low as Reasonably Practical"
      "AIS"
  , Acronym
      "ALERFA"
      "Alert phase"
      "AIS"
  , Acronym
      "ALI"
      "Airworthiness Limitation Item"
      ""
  , Acronym
      "ALM"
      "Aircraft Landing Minima"
      "AIS"
  , Acronym
      "ALR"
      "Alerting Message"
      "AIS"
  , Acronym
      "ALRS"
      "Approach lighting system"
      "ICAO"
  , Acronym
      "ALS"
      "Approach Lighting System"
      "AIS"
  , Acronym
      "ALT"
      "Altitude"
      "AIS"
  , Acronym
      "ALTN"
      "Alternate, Alternating (light alternates in colour)"
      "AIS"
  , Acronym
      "ALTN"
      "Alternate (aerodrome)"
      "AIS"
  , Acronym
      "AMA"
      "Area minimum altitude"
      "ICAO"
  , Acronym
      "AMC"
      "Acceptable Means of Compliance"
      ""
  , Acronym
      "AMD"
      "Amend, Amended"
      "AIS"
  , Acronym
      "AMDT"
      "Amendment (AIP amendment)"
      "ICAO/AIP"
  , Acronym
      "AME"
      "Aircraft Maintenance Engineer"
      "CASR Part 66"
  , Acronym
      "AMM"
      "Aircraft Maintenance Manual"
      "WATOG"
  , Acronym
      "AMO"
      "Approved Maintenance Organisation"
      "CASR Part 145"
  , Acronym
      "AMP"
      "Aircraft Maintenance Program"
      "CASR Part 145"
  , Acronym
      "AMPA"
      "Authorised Maintenance Personnel Assessor"
      "CASR Part 66"
  , Acronym
      "AMRD"
      "Australian Maintenance Requirements Document"
      "CASR Part 43"
  , Acronym
      "AMS"
      "Aircraft Maintenance Specialist"
      "CASR Part 145"
  , Acronym
      "AMSA"
      "Australian Maritime Safety Authority"
      ""
  , Acronym
      "AMSL"
      "Above Mean Sea Level"
      "AIS"
  , Acronym
      "AMSS"
      "Aeronautical mobile satellite service"
      "ICAO"
  , Acronym
      "AMT"
      "Aircraft Maintenance Technician"
      "CASR Part 66"
  , Acronym
      "ANA"
      "Air Navigation Act"
      ""
  , Acronym
      "ANC"
      "Aeronautical Chart 1:50,000 (followed by name/title)"
      "AIS"
  , Acronym
      "ANCS"
      "Aeronautical navigation chart - small scale (followed by name/title and scale)"
      "ICAO"
  , Acronym
      "ANO"
      "Air Navigation Order "
      ""
  , Acronym
      "ANS"
      "Answer"
      "ICAO"
  , Acronym
      "ANTA"
      "Australian National Training Authority"
      "CASR Part 66"
  , Acronym
      "ANVIS"
      "Aviator Night Vision Imaging Systems"
      ""
  , Acronym
      "AO"
      "Air Operator"
      ""
  , Acronym
      "AOC"
      "Air Operators Certificate"
      "CASR Part 119"
  , Acronym
      "AOC"
      "Aerodrome Obstacle Chart (followed by name/title)"
      "AIS/AIP"
  , Acronym
      "AOCM"
      "Air Operators Certification Manual"
      ""
  , Acronym
      "AOD"
      "Alcohol and Other Drug"
      "CASA"
  , Acronym
      "AOG"
      "Aircraft On Ground"
      "WATOG"
  , Acronym
      "AOPA"
      "Aircraft Owners and Pilots Association"
      ""
  , Acronym
      "AP"
      "Airport"
      "AIS"
  , Acronym
      "APAPI"
      "Abbreviated Precision Approach Path Indicator (pronounced 'AY-PAPI')"
      "AIS"
  , Acronym
      "APCH"
      "Approach"
      "AIS"
  , Acronym
      "APDC"
      "Aircraft Parking/Docking Chart (followed by name/title)"
      "AIS"
  , Acronym
      "APF"
      "Australian Parachute Federation"
      ""
  , Acronym
      "APMA"
      "Australian Parts Manufacturer Approval"
      "CASR Part 21"
  , Acronym
      "APN"
      "Apron"
      "ICAO/AIP"
  , Acronym
      "APP"
      "Approach Control"
      "AIS"
  , Acronym
      "APRX"
      "Approximate, Approximately"
      "AIS"
  , Acronym
      "APSG"
      "After Passing"
      "AIS"
  , Acronym
      "APU"
      "Auxillary power unit"
      "ICAO"
  , Acronym
      "APV"
      "Approach with Vertical Guidance"
      ""
  , Acronym
      "APV"
      "Approve, Approved, Approval"
      "AIS"
  , Acronym
      "AQZ"
      "Area QNH Zone"
      "AIS"
  , Acronym
      "ARC"
      "Area Chart"
      "ICAO/AIP"
  , Acronym
      "ARFFS"
      "Aerodrome Rescue and Fire Fighting Service (sometimes referred to as RFFS)"
      "CASR Part 139"
  , Acronym
      "ARFL"
      "Aeroplane Reference Field Length"
      "AIS"
  , Acronym
      "ARFO"
      "Area Forecast"
      ""
  , Acronym
      "ARH"
      "Aircraft Registration Holder"
      "CASR Part 47"
  , Acronym
      "ARM"
      "Airspace Risk Model"
      "ICAO/CASA"
  , Acronym
      "ARM"
      "Aircraft Recovery Manual"
      "WATOG"
  , Acronym
      "ARN"
      "Aviation Reference Number"
      "CASA/AIP"
  , Acronym
      "ARNG"
      "Arrange"
      "AIS"
  , Acronym
      "ARO"
      "Air traffic services reporting office"
      "ICAO"
  , Acronym
      "ARP"
      "Aerodrome reference point"
      "ICAO/AIP"
  , Acronym
      "ARP"
      "Air-Report (message type designator)"
      "ICAO/AIP"
  , Acronym
      "ARQ"
      "Automatic error correction"
      "ICAO"
  , Acronym
      "ARR"
      "Arrive, Arrival"
      "AIS"
  , Acronym
      "ARS"
      "Special Air-Report (message type designator)"
      "AIS"
  , Acronym
      "ARST"
      "Arresting (specify (part of) aircraft arresting equipment)"
      "ICAO"
  , Acronym
      "AS"
      "Altostratus (meteorological)"
      "ICAO/AIP"
  , Acronym
      "AS"
      "Australian Standard"
      ""
  , Acronym
      "ASA"
      "Australian Skydiving Association"
      ""
  , Acronym
      "ASAC"
      "Australian Sports Aviation Confederation"
      ""
  , Acronym
      "ASAP"
      "As Soon as Possible"
      "AIS"
  , Acronym
      "ASC"
      "Ascend to or ascending"
      "ICAO"
  , Acronym
      "ASDA"
      "Accelerate-stop distance available"
      "ICAO"
  , Acronym
      "ASE"
      "Airborne Support Equipment"
      "WATOG"
  , Acronym
      "ASE"
      "Altimetry System Error"
      "ICAO/AIP"
  , Acronym
      "ASEPTA"
      "Approved Single-engine Turbine Aeroplane"
      ""
  , Acronym
      "ASF"
      "Aviation Safety Forum"
      "AIS"
  , Acronym
      "ASH"
      "Aviation Services Handbook"
      ""
  , Acronym
      "ASI"
      "Air Speed Indicator"
      ""
  , Acronym
      "ASI"
      "Aviation Safety Index"
      ""
  , Acronym
      "ASIR"
      "Aviation Safety Incident Report"
      ""
  , Acronym
      "ASPH"
      "Asphalt"
      "ICAO"
  , Acronym
      "ASR"
      "Area Surveillance Radar"
      "AIS"
  , Acronym
      "ASRA"
      "Australian Sport Rotorcraft Association"
      ""
  , Acronym
      "ASSI"
      "Air Safety Support International- a wholly-owned subsidiary of the UK Civil Aviation Authority"
      "Air Safety Support International (UK)"
  , Acronym
      "ASTM"
      "American Society for Testing Materials"
      ""
  , Acronym
      "ASTRA"
      "Australian Strategic Air Traffic Management Group"
      ""
  , Acronym
      "AT-VASIS"
      "Abbreviated 'T' Visual Approach Slope Indicator System (pronounced             'AY-TEE-VASIS')"
      "AIS"
  , Acronym
      "AT..."
      "At (followed by time at which weather change is forecast to occur)"
      "ICAO"
  , Acronym
      "ATA"
      "Actual Time of Arrival"
      "AIS"
  , Acronym
      "ATA"
      "Air Transport Association"
      ""
  , Acronym
      "ATC"
      "Air Traffic Control (in general)"
      "ICAO/AIP"
  , Acronym
      "ATD"
      "Actual Time of Departure"
      "AIS"
  , Acronym
      "ATE"
      "Automatic Test Equipment"
      "WATOG"
  , Acronym
      "ATEL/ANAV"
      "Aeronautical Telecommunication and Radio Navigation Services"
      "CASR Part 171"
  , Acronym
      "ATFM"
      "Air Traffic Flow Management"
      "AIS"
  , Acronym
      "ATIS"
      "Automatic terminal information service"
      "ICAO"
  , Acronym
      "ATM"
      "Air traffic management"
      "ICAO/AIP"
  , Acronym
      "ATN"
      "Aeronautical telecommunications network"
      "ICAO"
  , Acronym
      "ATO"
      "Aviation Testing Officer"
      ""
  , Acronym
      "ATP"
      "At.. (time or place)"
      "ICAO"
  , Acronym
      "ATPL"
      "Airline Transport Pilot Licence"
      ""
  , Acronym
      "ATS"
      "Air Traffic Service(s)"
      ""
  , Acronym
      "ATSB"
      "Australian Transport Safety Bureau"
      ""
  , Acronym
      "ATSO"
      "Australian Technical Standard Order"
      "CASR Part 21"
  , Acronym
      "ATSOA"
      "Australian Technical Standard Order Authorisation"
      "CASR Part 21"
  , Acronym
      "ATTN"
      "Attention"
      "AIS"
  , Acronym
      "ATZ"
      "Aerodrome traffic zone"
      "ICAO"
  , Acronym
      "AUF"
      "Australian Ultralight Federation (now Recreational Aviation Australia)"
      ""
  , Acronym
      "AUTH"
      "Authorised, Authorisation"
      "AIS"
  , Acronym
      "AUW"
      "All Up Weight"
      "AIS"
  , Acronym
      "AUX"
      "Auxiliary"
      "AIS"
  , Acronym
      "AVBL"
      "Available"
      "AIS"
  , Acronym
      "AVG"
      "Average"
      "AIS"
  , Acronym
      "AVGAS"
      "Aviation Gasoline"
      "AIS"
  , Acronym
      "AVM"
      "Abrupt Vertical Manoeuvres (by the MIL)"
      "AIS"
  , Acronym
      "AWB"
      "Airworthiness Bulletin"
      ""
  , Acronym
      "AWIB"
      "Aerodrome Weather Information Broadcast"
      "AIS"
  , Acronym
      "AWK"
      "Aerial Work (Air work - general aviation)"
      "AIS"
  , Acronym
      "AWS"
      "Automatic Weather Station"
      "AIS"
  , Acronym
      "AWTA"
      "Advise as to what time is available"
      "ICAO"
  , Acronym
      "AWY"
      "Airway"
      "ICAO/AIP"
  , Acronym
      "AZM"
      "Azimuth"
      "ICAO/AIP"
  , Acronym
      "B"
      "Blue"
      "ICAO/AIP"
  , Acronym
      "BA"
      "Braking action"
      "ICAO"
  , Acronym
      "BASA"
      "Bilateral Aviation Safety Agreement"
      ""
  , Acronym
      "BASE"
      "Cloud Base (met)"
      "AIS"
  , Acronym
      "BCFG"
      "Fog Patches (met)"
      "AIS"
  , Acronym
      "BCN"
      "Beacon (aeronautical ground light)"
      "AIS"
  , Acronym
      "BCST"
      "Broadcast"
      "AIS"
  , Acronym
      "BDRY"
      "Boundary"
      "AIS"
  , Acronym
      "BECMG"
      "Becoming"
      "AIS"
  , Acronym
      "BETA"
      "Manually controlled mode for constant speed propellers or turbo prop aircraft"
      ""
  , Acronym
      "BFR"
      "Before"
      "AIS"
  , Acronym
      "BIP-M"
      "Basic Instruction Package for Meteorologists"
      ""
  , Acronym
      "BIPMT"
      "Basic Instruction Package for Meteorological Technicians"
      ""
  , Acronym
      "BITE"
      "Built In Test Equipment"
      "WATOG"
  , Acronym
      "BKN"
      "Broken (cloud descriptor) (met)"
      "AIS"
  , Acronym
      "BL..."
      "Blowing (followed by DU=dust, SA=sand or SN=snow)"
      "AIS"
  , Acronym
      "BLDG"
      "Building"
      "AIS"
  , Acronym
      "BLO"
      "Below Clouds"
      "AIS"
  , Acronym
      "BLW"
      "Below"
      "AIS"
  , Acronym
      "BOM"
      "Bureau of Meteorology"
      "AIS"
  , Acronym
      "BOMB"
      "Bombing"
      "AIS"
  , Acronym
      "BR"
      "Mist"
      "AIS"
  , Acronym
      "BRF"
      "Short (used to indicate type of approach)"
      "AIS"
  , Acronym
      "BRG"
      "Bearing"
      "AIS"
  , Acronym
      "BRKG"
      "Braking"
      "AIS"
  , Acronym
      "BRW"
      "Brakes release weight"
      ""
  , Acronym
      "BS"
      "Broadcasting Station (Commercial)"
      "AIS"
  , Acronym
      "BTL"
      "Between Layers"
      "AIS"
  , Acronym
      "BTN"
      "Between"
      "AIS"
  , Acronym
      "C"
      "Degrees Celsius (Centigrade)"
      "AIS"
  , Acronym
      "C"
      "Centre (Runway)"
      "AIS"
  , Acronym
      "CA"
      "Course Acquisition"
      ""
  , Acronym
      "CA/GRO"
      "Certified Air/Ground Radio Operator"
      "CASR Part 139"
  , Acronym
      "CA/GRS"
      "Certified Air/Ground Radio Service"
      "CASR Part 171"
  , Acronym
      "CAA NZ"
      "Civil Aviation Authority New Zealand"
      ""
  , Acronym
      "CAAP"
      "Civil Aviation Advisory Publication"
      "CAR 1988"
  , Acronym
      "CAGRO"
      "Certified Air Ground Radio Operation"
      "AIP"
  , Acronym
      "CAMO"
      "Certified Airworthiness Management Organisation"
      ""
  , Acronym
      "CAMP"
      "Continuous Airworthiness Maintenance Program"
      "CASR Part 145"
  , Acronym
      "CAO"
      "Civil Aviation Order"
      ""
  , Acronym
      "CAR "
      "Civil Aviation Regulations 1988"
      ""
  , Acronym
      "CAS"
      "Calibrated air speed"
      "ICAO"
  , Acronym
      "CASA"
      "Civil Aviation Safety Authority"
      ""
  , Acronym
      "CASR"
      "Civil Aviation Safety Regulations 1998"
      ""
  , Acronym
      "CAT"
      "Category"
      "AIS"
  , Acronym
      "CAT"
      "Clear Air Turbulence"
      "AIS"
  , Acronym
      "CAVOK"
      "Visibility, cloud and present weather better than prescribed values               or conditions"
      "AIS"
  , Acronym
      "CB"
      "Cumulonimbus (met)"
      "AIS"
  , Acronym
      "CBT"
      "Competency-based Training"
      ""
  , Acronym
      "CC"
      "Cirrocumulus (met)"
      "AIS"
  , Acronym
      "CCA"
      "Corrected meteorological message (for CCB, CCC..etc., in sequence)               (message type designator)"
      "ICAO"
  , Acronym
      "CCF"
      "Capability Check Flight"
      ""
  , Acronym
      "CCTS"
      "Circuits"
      "AIS"
  , Acronym
      "CD"
      "Candela"
      "ICAO"
  , Acronym
      "CDCCL"
      "Critical Design Configuration Control Limit"
      ""
  , Acronym
      "CDI"
      "Course Direction Indicator"
      ""
  , Acronym
      "CDL"
      "Configuration Deviation List"
      "CASR Part 43"
  , Acronym
      "CDN"
      "Coordination (message type designator)"
      "ICAO"
  , Acronym
      "CEN"
      "En Route and Area ATC unit"
      "AIS"
  , Acronym
      "CEO"
      "Chief Executive Officer"
      ""
  , Acronym
      "CET"
      "Clearance Expiry Time"
      "AIS"
  , Acronym
      "CF"
      "Change Frequency to"
      "AIS"
  , Acronym
      "CFI"
      "Chief Flying Instructor"
      "CAR 1988"
  , Acronym
      "CFIT"
      "Controlled Flight into Terrain"
      ""
  , Acronym
      "CFL"
      "Cleared Flight Level"
      "AIS"
  , Acronym
      "CFM"
      "Confirm, I confirm"
      "AIS"
  , Acronym
      "CG"
      "Centre of gravity"
      "ICAO"
  , Acronym
      "CGL"
      " Circling guidance light(s)"
      "ICAO"
  , Acronym
      "CH"
      "This is a channel continuity check of transmission to permit comparision of your record of channel-sequence numbers of messages received on the channel (to be used in AFS as a procedure signal)"
      "ICAO"
  , Acronym
      "CHG"
      "Modification (message type designator)"
      "ICAO"
  , Acronym
      "CHTR"
      "Charter"
      "AIS"
  , Acronym
      "CI"
      "Cirrus (met)"
      "AIS"
  , Acronym
      "CIDIN"
      "Common ICAO data interchange network"
      "ICAO"
  , Acronym
      "CIR"
      "Command Instrument Rating"
      "CAR 1988"
  , Acronym
      "CIT"
      "Near, Over Large Town(s)"
      "AIS"
  , Acronym
      "CIV"
      "Civil"
      "AIS"
  , Acronym
      "CK"
      "Check"
      "AIS"
  , Acronym
      "CL"
      "Centre Line"
      "AIS"
  , Acronym
      "CLA"
      "Clear type of ice formation"
      "AIS"
  , Acronym
      "CLBR"
      "Calibration"
      "AIS"
  , Acronym
      "CLD"
      "Cloud (met)"
      "AIS"
  , Acronym
      "CLG"
      "Calling"
      "AIS"
  , Acronym
      "CLIAS"
      "Climbing Indicated Airspeed"
      "AIS"
  , Acronym
      "CLMAX"
      "Maximum Co-efficient of Lift"
      ""
  , Acronym
      "CLR"
      "Clear, Cleared to... Clearance"
      "AIS"
  , Acronym
      "CLSD"
      "Closed, Close, Closing"
      "AIS"
  , Acronym
      "CM (cm)"
      "Centimetre"
      "AIS"
  , Acronym
      "CMB"
      "Climb to or Climbing to"
      "AIS"
  , Acronym
      "CMM"
      "Component Maintenance Manual"
      "WATOG"
  , Acronym
      "CMPL"
      "Configuration, Maintenance and Procedures"
      ""
  , Acronym
      "CMPL"
      "Completion, Completed, or Complete"
      "AIS"
  , Acronym
      "CMR"
      "Certification Maintenance Requirements"
      ""
  , Acronym
      "CMSD"
      "Commissioned"
      "AIS"
  , Acronym
      "CNL"
      "Cancel"
      "AIS"
  , Acronym
      "CNL"
      "Flight plan cancellation message"
      "AIS"
  , Acronym
      "CNS"
      "Communications, Navigation and Surveillance"
      "AIS"
  , Acronym
      "COA"
      "Certificates of Approval (issued to maintenance organisations)"
      "CAR 1988"
  , Acronym
      "CofA"
      "Certificate of Airworthiness"
      "CAR 1988"
  , Acronym
      "COM"
      "Communications"
      "AIS"
  , Acronym
      "CONC"
      "Concrete"
      "AIS"
  , Acronym
      "COND"
      "Condition"
      "AIS"
  , Acronym
      "CONS"
      "Continuous"
      "AIS"
  , Acronym
      "CONST"
      "Construction, Constructed"
      "AIS"
  , Acronym
      "CONT"
      "Continue(s), Continued"
      "AIS"
  , Acronym
      "COOR"
      "Coordinate, Coordinated"
      "AIS"
  , Acronym
      "COORD"
      "Coordinates"
      "AIS"
  , Acronym
      "COP"
      "Change over point"
      "ICAO"
  , Acronym
      "CoR"
      "Certificate of Registration"
      "CAR 1988"
  , Acronym
      "COR"
      "Correct, Corrected, Correction"
      "AIS"
  , Acronym
      "COS"
      "Conical Surface"
      "AIS"
  , Acronym
      "COT"
      "At the Coast, Coastal"
      "AIS"
  , Acronym
      "COV"
      "Cover, Covered, Covering"
      "AIS"
  , Acronym
      "CP"
      "Chief Pilot"
      "CAR 1988"
  , Acronym
      "CPDLC"
      "Controller-Pilot Data Link Communication"
      "ICAO/AIP"
  , Acronym
      "CPL"
      "Commercial Pilot Licence"
      "CAR 1988"
  , Acronym
      "CPL"
      "Current flight plan"
      "ICAO"
  , Acronym
      "CRC"
      "Cyclic redundancy check"
      "ICAO"
  , Acronym
      "CRIS"
      "Cost Recovery Impact Statement"
      "Department of Finance"
  , Acronym
      "CRM"
      "Crew Resource Management"
      "CASR Part 121"
  , Acronym
      "CRZ"
      "Cruise"
      "AIS"
  , Acronym
      "CS"
      "Cirrostratus (met)"
      "AIS"
  , Acronym
      "CS"
      "Call sign"
      "AIS"
  , Acronym
      "CSDD"
      "Common Support Data Dictionary published by the Air Transport Association of America (ATA).  The CSDD replaces the WATOG."
      ""
  , Acronym
      "CSU"
      "Constant Speed Unit"
      ""
  , Acronym
      "CTA"
      "Control Area"
      "AIS"
  , Acronym
      "CTAF"
      "Common Traffic Advisory Frequency"
      "CASR Part 172/AIP"
  , Acronym
      "CTAF(R)"
      "Common Traffic Advisory Frequency - (Radio)"
      "AIS"
  , Acronym
      "CTAM"
      "Climb to and maintain"
      "ICAO"
  , Acronym
      "CTC"
      "Category Technical Competency"
      "CAR 1988"
  , Acronym
      "CTC"
      "Contact"
      "ICAO/AIP"
  , Acronym
      "CTL"
      "Control"
      "AIS"
  , Acronym
      "CTN"
      "Caution"
      "AIS"
  , Acronym
      "CTR"
      "Control Zone"
      "AIP"
  , Acronym
      "CU"
      "Cumulus (met)"
      "AIS"
  , Acronym
      "CUF"
      "Cumuliform (met)"
      "AIS"
  , Acronym
      "CUST"
      "Customs"
      "AIS"
  , Acronym
      "CVR"
      "Cockpit voice recorder"
      "ICAO/AIP"
  , Acronym
      "CW"
      "Carrier Wave"
      "ICAO"
  , Acronym
      "CWY"
      "Clearway"
      "AIS"
  , Acronym
      "D"
      "Deleted"
      "AIS"
  , Acronym
      "D"
      "Downward (tendency in RVR (Runway visual range) during previous               10 minutes)"
      "ICAO"
  , Acronym
      "D-ATIS"
      "Data Link Automatic Terminal Information Service (pronounced'DEE-ATIS')"
      "AIS"
  , Acronym
      "D-VOLMET"
      "Data Link VOLMET"
      "AIS"
  , Acronym
      "D..."
      "Danger Area (followed by identification)"
      "AIS"
  , Acronym
      "DA"
      "Decision Altitude"
      "AIS"
  , Acronym
      "DA/H"
      "Decision Altitude/Height"
      "AIP"
  , Acronym
      "DALR"
      "Dry Adiabatic Lapse Rate"
      ""
  , Acronym
      "DAME"
      "Designated Aviation Medical Examiner"
      "CASR Part 67"
  , Acronym
      "DAO"
      "Designated Aviation Ophthamologist"
      "CASR Part 67"
  , Acronym
      "DAP"
      "Departure and Approach Procedures"
      "AIP"
  , Acronym
      "DCD"
      "Double channel duplex"
      "ICAO"
  , Acronym
      "DCKG"
      "Docking"
      "AIS"
  , Acronym
      "DCMSD"
      "Decommissioned"
      "AIS"
  , Acronym
      "DCPC"
      "Direct Controller-Pilot Communications"
      "AIS"
  , Acronym
      "DCS"
      "Double channel simplex"
      "ICAO"
  , Acronym
      "DCT"
      "Direct (in relation to flight plan clearances and type of approach)"
      "AIS"
  , Acronym
      "DE"
      "Form (used to preceed the call sign of the calling station) (to be used in AFS as a procedure signal)&lt;/em&gt;"
      "ICAO/AIP"
  , Acronym
      "DEC"
      "December"
      "AIS"
  , Acronym
      "DEG"
      "Degrees"
      "AIS"
  , Acronym
      "DEP"
      "Depart, Departure, Departed, Departing, Departure Message"
      "AIS"
  , Acronym
      "DER"
      "Departure End of Runway"
      "AIP"
  , Acronym
      "DER"
      "Designated Engineering Representative"
      "FAR"
  , Acronym
      "DES"
      "Descend to, Descending to"
      "AIS"
  , Acronym
      "DEST"
      "Destination"
      "AIS"
  , Acronym
      "DETRESFA"
      "Distress Phase"
      "AIS"
  , Acronym
      "DEV"
      "Deviation, Deviating"
      "AIS"
  , Acronym
      "DF"
      "Direction Finder/ Finding"
      "AIS"
  , Acronym
      "DFDR"
      "Digital Flight Data Recorder"
      "AIS"
  , Acronym
      "DFTI"
      "Distance from touchdown indicator"
      "ICAO"
  , Acronym
      "DGA"
      "Distance Measuring Equipment or Global Positioning Satellite System Arrival"
      ""
  , Acronym
      "DH"
      "Decision Height"
      "ICAO"
  , Acronym
      "DIF"
      "Diffuse"
      "AIS"
  , Acronym
      "DISP"
      "Displaced"
      "AIS"
  , Acronym
      "DIST"
      "Distance"
      "AIS"
  , Acronym
      "DIV"
      "Diversion, Divert, Diverting"
      "AIS"
  , Acronym
      "DLA"
      "Delay (message type designator)"
      "ICAO/AIP"
  , Acronym
      "DLIC"
      "Data Link Initiation Capability"
      "AIS"
  , Acronym
      "DLY"
      "Daily"
      "AIS"
  , Acronym
      "DMC"
      "Direct Maintenance Cost"
      "WATOG"
  , Acronym
      "DME"
      "Distance Measuring Equipment"
      "ICAO/AIP"
  , Acronym
      "DMEN"
      "DME (International)"
      "AIS"
  , Acronym
      "DMEP"
      "DME (International Precision used in conjunction with Microwave Landing System)"
      "AIS"
  , Acronym
      "DNG"
      "Danger, Dangerous"
      "AIS"
  , Acronym
      "DOC"
      "Direct Operating Cost"
      "WATOG"
  , Acronym
      "DOC"
      "Documents"
      "AIS"
  , Acronym
      "DoD"
      "Department of Defence"
      ""
  , Acronym
      "DOM"
      "Domestic"
      "AIS"
  , Acronym
      "DP"
      "Dew Point Temperature"
      "ICAO/AIP"
  , Acronym
      "DP"
      "Discussion Paper"
      "CASA"
  , Acronym
      "DPT"
      "Depth"
      "AIS"
  , Acronym
      "DR"
      "Dead Reckoning (navigation)"
      "ICAO"
  , Acronym
      "DR..."
      "Low drifting (followed by DU=dust, SA=sand or SN=snow)"
      "AIS"
  , Acronym
      "DRG"
      "During"
      "AIS"
  , Acronym
      "DS"
      "Dust storm"
      "AIS"
  , Acronym
      "DSB"
      "Double sideband"
      "ICAO"
  , Acronym
      "DTAM"
      "Descend to And Maintain"
      "AIS"
  , Acronym
      "DTG"
      "Date-Time Group"
      "AIS"
  , Acronym
      "DTHR"
      "Displaced Runway Threshold"
      "AIS"
  , Acronym
      "DTRT"
      "Deteriorate, Deteriorating"
      "AIS"
  , Acronym
      "DTW"
      "Dual tandem wheels"
      "ICAO"
  , Acronym
      "DU"
      "Dust"
      "AIS"
  , Acronym
      "DUC"
      "Dense Upper Cloud (met)"
      "AIS"
  , Acronym
      "DUPE"
      "This is a duplicate message (to be used in Aeronautical Fixed Service as a procedure signal)"
      "ICAO"
  , Acronym
      "DUR"
      "Duration"
      "AIS"
  , Acronym
      "DVOR"
      "Doppler VOR"
      "AIS"
  , Acronym
      "DW"
      "Dual wheels"
      "ICAO"
  , Acronym
      "DZ"
      "Drizzle"
      "AIS"
  , Acronym
      "E"
      "East, East Longitude"
      "AIS"
  , Acronym
      "E E E"
      "Error (to be used in AFS as a procedure signal)"
      "ICAO"
  , Acronym
      "EASA"
      "European Aviation Safety Agency"
      ""
  , Acronym
      "EAT"
      "Expected Approach Time"
      "AIS"
  , Acronym
      "EB"
      "Eastbound"
      "AIS"
  , Acronym
      "ECAR"
      "European Civil Aviation Requirement (formerly JAR)"
      ""
  , Acronym
      "EDTO"
      "Extended Diversion Time Operations"
      ""
  , Acronym
      "EET"
      "Estimated Elapsed Time"
      "AIS"
  , Acronym
      "EFB"
      "Electronic Flight Bag"
      ""
  , Acronym
      "EFC"
      "Expect further clearance"
      "ICAO"
  , Acronym
      "EGNOS"
      "European Geostationary Navigation Overlay System"
      ""
  , Acronym
      "EGPWS"
      "Enhanced Ground Proximity Warning System"
      "CAR 1988"
  , Acronym
      "EHF"
      "Extremely high frequency (30,000 to 300,000 MHz)"
      "ICAO/AIP"
  , Acronym
      "EHR"
      "Encumbrance Holder Register"
      "CASR Part 47"
  , Acronym
      "ELBA"
      "Emergency locator beacon-aircraft"
      "ICAO"
  , Acronym
      "ELEV"
      "Elevation"
      "AIS"
  , Acronym
      "ELR"
      "Extra Long Range"
      "AIS"
  , Acronym
      "ELT(S)"
      "Emergency Locator Transmitter (Survival)"
      "CAR 1988"
  , Acronym
      "EM"
      "Engine Manual"
      "WATOG"
  , Acronym
      "EM"
      "Explanatory Memorandum (submitted to EXCO with regulation amendment)"
      ""
  , Acronym
      "EM"
      "Emission"
      "AIS"
  , Acronym
      "EMBD"
      "Embedded in a Layer (to indicate cumulonimbus embedded in layers               of other clouds)"
      "AIS"
  , Acronym
      "EMERG"
      "Emergency"
      "AIS"
  , Acronym
      "EMS"
      "Emergency Medical Service"
      ""
  , Acronym
      "EMS"
      "Environment Management System"
      ""
  , Acronym
      "END"
      "Stop-end (related to RVR)"
      "ICAO"
  , Acronym
      "ENDCE"
      "Endurance"
      "AIS"
  , Acronym
      "ENE"
      "East North-East"
      "AIS"
  , Acronym
      "ENG"
      "Engine"
      "AIS"
  , Acronym
      "ENR"
      "En Route"
      "AIS"
  , Acronym
      "ENRC"
      "En Route Chart (followed by name/title)"
      "AIS"
  , Acronym
      "EOBT"
      "Estimated off Blocks Time"
      "AIS"
  , Acronym
      "EPA"
      "Environment Protection Agency"
      ""
  , Acronym
      "EPIRB"
      "Electronic Position Indicating Radio Beacon (marine term.)"
      "AIS"
  , Acronym
      "EQPT"
      "Equipment"
      "AIS"
  , Acronym
      "ER"
      "Here..or herewith"
      "ICAO"
  , Acronym
      "ERC"
      "En-route Chart"
      "AIS"
  , Acronym
      "ERP"
      "Emergency Response Plan"
      ""
  , Acronym
      "ERSA"
      "En-route Supplement Australia"
      "AIP"
  , Acronym
      "ES"
      "Explanatory Statement (tabled with regulations in Parliament)"
      ""
  , Acronym
      "ESD"
      "Electrostatic sensitive discharge"
      ""
  , Acronym
      "ESDE"
      "ESD equipment"
      ""
  , Acronym
      "ESE"
      "East Southeast"
      "AIS"
  , Acronym
      "ESIR"
      "Electronic safety incident report"
      ""
  , Acronym
      "EST"
      "Estimate or estimated or estimate (message type designator)"
      "AIS"
  , Acronym
      "ETA"
      "Estimated Time of Arrival, Estimating Arrival"
      "AIS"
  , Acronym
      "ETCM"
      "Engine trend and condition monitoring"
      ""
  , Acronym
      "ETD"
      "Estimated Time of Departure, Estimating Departure"
      "AIS"
  , Acronym
      "ETO"
      "Estimated Time Over significant point"
      "AIS"
  , Acronym
      "ETOPS"
      "Extended range operations by turbine-engined aeroplanes"
      "ICAO"
  , Acronym
      "ETP"
      "Equi Time Interval"
      ""
  , Acronym
      "ETT"
      "Estimated Time Interval"
      ""
  , Acronym
      "EV"
      "Every"
      "AIS"
  , Acronym
      "EVU"
      "Enforceable Voluntary Undertaking"
      "Civil Aviation Act 1988 "
  , Acronym
      "EXC"
      "Except"
      "AIS"
  , Acronym
      "EXCO"
      "Executive Council"
      ""
  , Acronym
      "EXER"
      "Exercises, Exercising, to exercise"
      "AIS"
  , Acronym
      "EXP"
      "Expect, Expected, Expecting"
      "AIS"
  , Acronym
      "EXTD"
      "Extend, Extending, Extended"
      "AIS"
  , Acronym
      "F"
      "Fixed (chart symbol)"
      "AIS"
  , Acronym
      "FAA"
      "Federal Aviation Administration of the USA"
      ""
  , Acronym
      "FAAOC"
      "Foreign Aircraft Air Operator's Certificate"
      "CASR Part 129"
  , Acronym
      "FAC"
      "Facility, Facilities"
      "AIS"
  , Acronym
      "FADEC"
      "Full Authority Digital Engine Control"
      ""
  , Acronym
      "FAF"
      "Final Approach Fix"
      "AIP/ICAO"
  , Acronym
      "FAL"
      "Facilitation of International Air"
      "AIS"
  , Acronym
      "FAP"
      "Final Approach Point"
      "AIP/ICAO"
  , Acronym
      "FARs"
      "Federal Aviation Regulations (Federal Aviation Administration of the USA)"
      ""
  , Acronym
      "FATO"
      "Final approach and take-off area"
      "ICAO/AIP"
  , Acronym
      "FAWP"
      "Final Approach Waypoint"
      "AIS"
  , Acronym
      "FAX"
      "Facsimile Transmission"
      "AIS"
  , Acronym
      "FBL"
      "Light (used to indicate the intensity of WX phenomena, interference               or static reports, eg BL RA = light rain)"
      "AIS"
  , Acronym
      "FC"
      "Funnel Cloud (tornado or water spout)"
      "AIS"
  , Acronym
      "FCST"
      "Forecast"
      "AIS"
  , Acronym
      "FCT"
      "Friction coefficient"
      "ICAO"
  , Acronym
      "FD"
      "Fault Detection "
      ""
  , Acronym
      "FDE"
      "Fault detection and exclusion (in relation to GPS equipment)"
      ""
  , Acronym
      "FDL"
      "Fixed Distance Lighting"
      "AIS"
  , Acronym
      "FDPS"
      "Flight Data Processing system"
      "AIS"
  , Acronym
      "FDS"
      "Flight Director System"
      ""
  , Acronym
      "FEW"
      "Few (cloud descriptor)"
      "AIS"
  , Acronym
      "FFR"
      "Flood, Fire Relief"
      "AIS"
  , Acronym
      "FG"
      "Fog"
      "AIS"
  , Acronym
      "FIA"
      "Flight Information Area"
      "AIS"
  , Acronym
      "FIC"
      "Flight Information Centre"
      "AIS"
  , Acronym
      "FIO"
      "Flight Information Office"
      "AIS"
  , Acronym
      "FIR"
      "Flight Information Region"
      "ICAO/AIP"
  , Acronym
      "FISA"
      "Flight information service (automated)"
      "ICAO"
  , Acronym
      "FISS"
      "Flight Information Service Station"
      "AIS"
  , Acronym
      "FIX"
      "Fault Isolation Manual"
      "WATOG"
  , Acronym
      "FL"
      "Flight Level"
      "ICAO/AIP"
  , Acronym
      "FLD"
      "Field"
      "AIS"
  , Acronym
      "FLG"
      "Flashing"
      "AIS"
  , Acronym
      "FLIR"
      "Forward Looking Infra Red"
      ""
  , Acronym
      "FLR"
      "Flares"
      "AIS"
  , Acronym
      "FLT"
      "Flight"
      "ICAO/WATOG/AIP"
  , Acronym
      "FLTCK"
      "Flight check"
      "ICAO/AIP"
  , Acronym
      "FLUC"
      "Fluctuating, Fluctuation, Fluctuated"
      "AIS"
  , Acronym
      "FLW"
      "Follow(s), Following"
      "AIS"
  , Acronym
      "FLY"
      "Fly, Flying"
      "AIS"
  , Acronym
      "FM"
      "From"
      "AIS"
  , Acronym
      "FM..."
      "From (followed by time weather change is forecast to begin)"
      "AIS"
  , Acronym
      "FMS"
      "Flight Management System"
      "ICAO"
  , Acronym
      "FMU"
      "Flow Management Unit"
      "AIS"
  , Acronym
      "FNA"
      "Final Approach"
      "AIS"
  , Acronym
      "FNPT"
      "Flight and Navigation Procedures Trainer"
      "ICAO"
  , Acronym
      "FOD"
      "Foreign Object Damage"
      "WATOG"
  , Acronym
      "FOI"
      "Flying Operations Inspector"
      ""
  , Acronym
      "FPD"
      "Flight Plan Designator"
      "AIS"
  , Acronym
      "FPL"
      "Filed Flight Plan Message "
      "AIS"
  , Acronym
      "FPM"
      "Feet per Minute"
      "AIS"
  , Acronym
      "FPR"
      "Flight Plan Route"
      "AIS"
  , Acronym
      "FR"
      "Fuel Remaining"
      "AIS"
  , Acronym
      "FREQ"
      "Frequency"
      "AIS"
  , Acronym
      "FRM"
      "Fault Reporting Manual"
      "WATOG"
  , Acronym
      "FRMS"
      "Fatigue Risk Management System"
      ""
  , Acronym
      "FRNG"
      "Firing"
      "AIS"
  , Acronym
      "FRONT"
      "Front (relating to weather)"
      "ICAO"
  , Acronym
      "FRQ"
      "Frequent"
      "AIS"
  , Acronym
      "FS"
      "Flight Service (in general)"
      "AIS"
  , Acronym
      "FSA"
      "Flight Safety Australia"
      ""
  , Acronym
      "FSL"
      "Full Stop Landing"
      "AIS"
  , Acronym
      "FSP"
      "Fish Spotting"
      "AIS"
  , Acronym
      "FSS"
      "Flight service station"
      "ICAO"
  , Acronym
      "FST"
      "First"
      "AIS"
  , Acronym
      "FT"
      "Feet"
      "AIS"
  , Acronym
      "FTD"
      "Flight training device"
      "CASR Part 60"
  , Acronym
      "FTE"
      "Full time equivalent"
      ""
  , Acronym
      "FTO"
      "Flight Training Organisation"
      ""
  , Acronym
      "FU"
      "Smoke"
      "AIS"
  , Acronym
      "FXD"
      "Fixed"
      "AIS"
  , Acronym
      "FZ"
      "Freezing"
      "AIS"
  , Acronym
      "FZDZ"
      "Freezing Drizzle"
      "AIS"
  , Acronym
      "FZFG"
      "Freezing Fog"
      "AIS"
  , Acronym
      "FZL"
      "Freezing Level"
      "AIS"
  , Acronym
      "FZRA"
      "Freezing Rain"
      "AIS"
  , Acronym
      "G"
      "Gravity"
      ""
  , Acronym
      "G"
      "Green"
      "AIS"
  , Acronym
      "G/A"
      "Ground-to-air"
      "ICAO"
  , Acronym
      "G/A/G"
      "Ground-to-air and air-to-ground"
      "ICAO"
  , Acronym
      "GA"
      "Go ahead, resume sending (to be used in AFS as a procedure signal)"
      "ICAO"
  , Acronym
      "GA"
      "General Aviation"
      "CAR 1988"
  , Acronym
      "GAAP"
      "General Aviation Aerodrome Procedures"
      "AIS"
  , Acronym
      "GAMET"
      "Area forecast for low-level flights"
      "ICAO"
  , Acronym
      "GAPAN"
      "Honourable Company of Air Pilots, Australia, Inc."
      ""
  , Acronym
      "GBAS"
      "Ground Based Augmentation System"
      ""
  , Acronym
      "GCA"
      "Ground Controlled Approach"
      "AIS"
  , Acronym
      "GCN"
      "General Computing Network (operated by Airservices Australia)"
      "AIS"
  , Acronym
      "GDOP"
      "Geometric Dilution of Precision"
      ""
  , Acronym
      "GEN"
      "General"
      "AIS"
  , Acronym
      "GEO"
      "Geographic, true"
      "AIS"
  , Acronym
      "GES"
      "Ground Earth Station"
      "AIS"
  , Acronym
      "GFA"
      "The Gliding Federation of Australia"
      ""
  , Acronym
      "GFY"
      "Glider Flying"
      "AIS"
  , Acronym
      "GHA"
      "Ground handling agents"
      ""
  , Acronym
      "GIVD"
      "Gravity Induced Vestibular Dysfunction"
      ""
  , Acronym
      "GLD"
      "Glider"
      "AIS"
  , Acronym
      "GLOC"
      "Gravity Induced Loss of Consciousness"
      ""
  , Acronym
      "GLONASS"
      "Global Orbiting Navigation Satellite System (pronounced 'GLO-NAS')"
      "AIS"
  , Acronym
      "GM"
      "Guidance Material"
      ""
  , Acronym
      "GMC"
      "Ground Movement Chart (followed by name/title)"
      "AIS"
  , Acronym
      "GND"
      "Ground"
      "AIS"
  , Acronym
      "GNDCK"
      "Ground Check"
      "AIS"
  , Acronym
      "GNS"
      "Global Navigation System"
      "AIS"
  , Acronym
      "GNSS"
      " Global navigation satellite system"
      "ICAO/AIP"
  , Acronym
      "GP"
      "Guiding principles"
      "CAR 1998"
  , Acronym
      "GP"
      "Glide Path"
      "ICAO/AIP"
  , Acronym
      "GP"
      "FLG Group Flashing (number) (used in conjunction with aerodrome lighting)"
      "AIS"
  , Acronym
      "GPI"
      "Glide Path Intercept"
      "AIS"
  , Acronym
      "GPS"
      "Global positioning system (see GNSS)"
      ""
  , Acronym
      "GPWS"
      "Ground proximity warning system"
      ""
  , Acronym
      "GR"
      "Hail"
      "AIS"
  , Acronym
      "GRAD"
      "Minimum Required Climb Gradient"
      "AIS"
  , Acronym
      "GRAS"
      "Ground Based Regional Augmentation System"
      ""
  , Acronym
      "GRASS"
      "Grass Landing Area"
      "AIS"
  , Acronym
      "GRIB"
      "Processed meteoroglogical data in the form of grid point values expressed in binary form (aeronautical meterological code)"
      "ICAO/AIP"
  , Acronym
      "GRVL"
      "Gravel"
      "AIS"
  , Acronym
      "GS"
      "Groundspeed"
      "AIS"
  , Acronym
      "GS"
      "Small Hail and/or Snow Pellets"
      "AIS"
  , Acronym
      "GSE"
      "Ground Support Equipment"
      "WATOG"
  , Acronym
      "GUND"
      "Geoid Undulation"
      "AIS"
  , Acronym
      "H24"
      "Continuous day and night service"
      "AIS"
  , Acronym
      "HAA"
      "Height Above Aerodrome"
      "AIS"
  , Acronym
      "HAA"
      "Helicopter Association of Australia"
      ""
  , Acronym
      "HAAMC"
      "Head of Aircraft Airworthiness and Maintenance Control"
      "Aviation Law in Australia, 2004 edition"
  , Acronym
      "HAM"
      "Head of Aircraft Maintenance"
      "CASR Part 145"
  , Acronym
      "HAMC"
      "Head of Aircraft Maintenance Control"
      "CASR Part 145"
  , Acronym
      "HAPI"
      "Helicopter approach path indicator"
      "ICAO"
  , Acronym
      "HAT"
      "Height Above Threshold"
      "AIS"
  , Acronym
      "HBN"
      "Hazard Beacon"
      "AIS"
  , Acronym
      "HDF"
      "High frequency direction-finding station"
      "ICAO"
  , Acronym
      "HDG"
      "Heading"
      "AIS"
  , Acronym
      "HDS"
      "Hours of Daylight Saving"
      "AIS"
  , Acronym
      "HEL"
      "Helicopter"
      "AIS"
  , Acronym
      "HF"
      "Human Factors"
      ""
  , Acronym
      "HF"
      "High Frequency (3000 to 30,000 KHZ)"
      "AIS"
  , Acronym
      "HFEM"
      "Human Factor and Error Management System"
      ""
  , Acronym
      "HGFA"
      "Hang Gliding Federation of Australia Inc"
      ""
  , Acronym
      "HGS"
      "Heads-up Guidance System"
      ""
  , Acronym
      "HGT"
      "Height, Height Above"
      "AIS"
  , Acronym
      "HIAL"
      "High Intensity Approach Lighting"
      "AIS"
  , Acronym
      "HIOL"
      "High Intensity Obstacle Lights"
      "AIS"
  , Acronym
      "HIRL"
      "High Intensity Runway Lighting"
      "AIS"
  , Acronym
      "HJ"
      "Sunrise to Sunset"
      "AIS"
  , Acronym
      "HLDG"
      "Holding"
      "AIS"
  , Acronym
      "HLS"
      "Helicopter Landing Site"
      "AIS"
  , Acronym
      "HMI"
      "Human-machine Interface"
      ""
  , Acronym
      "HN"
      "Sunset to Sunrise"
      "AIS"
  , Acronym
      "HO"
      "Service available to meet operational requirements"
      "AIS"
  , Acronym
      "HOFO"
      "Head of Flying Operations"
      ""
  , Acronym
      "HOO"
      "Head of Operations"
      ""
  , Acronym
      "HOSP"
      "Hospital Aircraft"
      ""
  , Acronym
      "HOTAS"
      "Head of Training and Standards"
      "AIS"
  , Acronym
      "HOTC"
      "Head of Training and Checking"
      ""
  , Acronym
      "HP"
      "Horse Power"
      ""
  , Acronym
      "HPA"
      "HectoPascal"
      ""
  , Acronym
      "HR"
      "Hours"
      "AIS"
  , Acronym
      "HS"
      "Homestead"
      "AIS"
  , Acronym
      "HS"
      "Service available during hours of scheduled operations"
      "AIS"
  , Acronym
      "HSL"
      "Hold Short Lights"
      "AIS"
  , Acronym
      "HUD "
      "Head Up Display"
      "AIS"
  , Acronym
      "HURCN"
      "Hurricane"
      ""
  , Acronym
      "HVDF"
      "High and very high frequency direction finding stations (at the same location)"
      "AIS"
  , Acronym
      "HVY"
      "Heavy"
      "AIS"
  , Acronym
      "HVY"
      "Heavy (used to indicate the intensity of WX phenomena, eg HVY               RA = heavy rain)"
      "AIS"
  , Acronym
      "HX"
      "No specific working hours"
      "AIS"
  , Acronym
      "HYR"
      "Higher"
      "AIS"
  , Acronym
      "HZ"
      "Haze"
      "AIS"
  , Acronym
      "HZ (hz)"
      "Hertz"
      "AIS"
  , Acronym
      "HZS"
      "Horizontal Surface"
      "AIS"
  , Acronym
      "IA"
      "Inspection authorisation"
      "AIS"
  , Acronym
      "IAC"
      "Instrument approach chart (followed by name/title)"
      "FAR"
  , Acronym
      "IAF"
      "Initial Approach Fix"
      "ICAO/AIP"
  , Acronym
      "IAL"
      "Instrument Approach and Landing Chart"
      "AIS"
  , Acronym
      "IAO"
      "In and out of clouds"
      "AIS"
  , Acronym
      "IAP"
      "Instrument Approach Plate"
      "AIS"
  , Acronym
      "IAR"
      "Intersection of air routes"
      ""
  , Acronym
      "IAS"
      "Indicated air speed"
      "ICAO"
  , Acronym
      "IATA"
      "International Air Transport Association"
      "ICAO"
  , Acronym
      "IAW"
      "In Accordance With"
      ""
  , Acronym
      "IAWP"
      "Initial Approach Way-point"
      ""
  , Acronym
      "IBN"
      "Identification Beacon"
      "AIS"
  , Acronym
      "IC"
      "Ice Crystals (MET code)"
      "AIS"
  , Acronym
      "ICA"
      "Instructions for continuing airworthiness (issued by manufacturer or designer)"
      "AIS"
  , Acronym
      "ICAO"
      "International Civil Aviation Organization"
      "CAR 1998"
  , Acronym
      "ICE"
      "Icing, Ice"
      "ICAO"
  , Acronym
      "ICUS"
      "In-command Under Supervision"
      "AIS"
  , Acronym
      "ID"
      "Identifier, identify"
      ""
  , Acronym
      "IDENT"
      "Identification"
      "AIS"
  , Acronym
      "IF"
      "Instrument Flight"
      "AIS"
  , Acronym
      "IF"
      "Intermediate Approach Fix"
      ""
  , Acronym
      "IFF"
      "Identification Friend/Foe"
      "AIS"
  , Acronym
      "IFR"
      "Instrument flight rules"
      "AIS"
  , Acronym
      "IFSD"
      "In-flight Shutdown"
      "ICAO/AIP"
  , Acronym
      "IG"
      "Implementation Group"
      ""
  , Acronym
      "IGA"
      "International general aviation"
      ""
  , Acronym
      "ILS"
      "Instrument Landing System"
      "/AIPICAO"
  , Acronym
      "IM"
      "Inner Marker"
      "AIS"
  , Acronym
      "IMC"
      "Instrument meteorological conditions (other than VMC)"
      "AIS"
  , Acronym
      "IMC"
      "Indirect Maintenance Cost"
      "CAR 1988"
  , Acronym
      "IMG"
      "Immigration"
      "WATOG"
  , Acronym
      "IMI"
      "Interrogation sign"
      "AIS"
  , Acronym
      "IMPR"
      "Improve, Improving, Improvement"
      "ICAO"
  , Acronym
      "IMT"
      "Immediate, Immediately"
      "AIS"
  , Acronym
      "INA"
      "Initial approach"
      "AIS"
  , Acronym
      "INBD"
      "Inbound"
      "ICAO"
  , Acronym
      "INC"
      "In Cloud"
      "AIS"
  , Acronym
      "INCERFA"
      "Uncertainty Phase"
      "AIS"
  , Acronym
      "INFO"
      "Information"
      "AIS"
  , Acronym
      "INOP"
      "Inoperative"
      "AIS"
  , Acronym
      "INP"
      "If not possible"
      "AIS"
  , Acronym
      "INPR"
      "In progress"
      "ICAO"
  , Acronym
      "INS"
      "Inertial Navigation System"
      "ICAO"
  , Acronym
      "INSTL"
      "Install, Installed, Installation"
      "AIS"
  , Acronym
      "INSTR"
      "Instrument"
      "AIS"
  , Acronym
      "INT"
      "Intersection"
      "AIS"
  , Acronym
      "INTER"
      "Intermittent, Intermittently (meteorological)"
      "AIS"
  , Acronym
      "INTL"
      "International"
      "AIS"
  , Acronym
      "INTRG"
      "Interrogator"
      "AIS"
  , Acronym
      "INTRP"
      "Interrupt, Interruption, Interrupted"
      "AIS"
  , Acronym
      "INTSF"
      "Intensify, Intensifying"
      "AIS"
  , Acronym
      "INTST"
      "Intensity"
      "AIS"
  , Acronym
      "IoA"
      "Instrument of appointment"
      "AIS"
  , Acronym
      "IOC"
      "Indirect Operating Cost"
      "CAR 1988"
  , Acronym
      "IOE"
      "Initial Operational Experience"
      "WATOG"
  , Acronym
      "IPC"
      "Illustrated Parts Catalogue"
      ""
  , Acronym
      "IR"
      "Ice on runway"
      "WATOG"
  , Acronym
      "IREX"
      "Instrument Training Examination"
      "ICAO"
  , Acronym
      "IRM"
      "Immediately Reportable Matters"
      "ATSB"
  , Acronym
      "IRS"
      "Internal Reporting System"
      ""
  , Acronym
      "ISA"
      "International Standard Atmosphere"
      ""
  , Acronym
      "ISB"
      "Independent Sideband"
      "AIS"
  , Acronym
      "ISIM"
      "Integrated Safety Investigation Methodology"
      "CASA"
  , Acronym
      "ISO9000"
      "International Organization for Standardization"
      ""
  , Acronym
      "ISOL"
      "Isolated"
      ""
  , Acronym
      "ITT"
      "Interstage Turbine Temperature"
      "AIS"
  , Acronym
      "IWI"
      "Illuminated Wind Indicator"
      ""
  , Acronym
      "IWP"
      "Intermediate Way-point"
      "AIS"
  , Acronym
      "J-BAR"
      "Jet Barrier"
      "AIS"
  , Acronym
      "JAA"
      "Joint Aviation Authorities (of Europe)"
      "AIS"
  , Acronym
      "JAR"
      "Joint Aviation Requirements (of the European JAA)"
      ""
  , Acronym
      "JAR-OPS"
      "JAR Operations"
      "AIS"
  , Acronym
      "JTSO"
      "Joint Technical Standard Order"
      "AIS"
  , Acronym
      "JTST"
      "Jet Stream"
      "AIS"
  , Acronym
      "KDR"
      "Knowledge Deficiency Report"
      "AIS"
  , Acronym
      "KG (kg)"
      "Kilograms"
      "AIS"
  , Acronym
      "KHZ (kHz)"
      "Kilohertz"
      ""
  , Acronym
      "KM (km)"
      "Kilometers"
      "AIS"
  , Acronym
      "KMH (km/h)"
      "Kilometers per hour"
      "AIS"
  , Acronym
      "KPA (kPa)"
      "Kilopascals"
      "AIS"
  , Acronym
      "KT (kt)"
      "Knots"
      "AIS"
  , Acronym
      "KW (kw)"
      "Kilowatts"
      "AIS"
  , Acronym
      "L"
      "Left (runway identification)"
      "AIS"
  , Acronym
      "L"
      "Locator (see LM, LO)"
      "AIS"
  , Acronym
      "LAAS"
      "Local Area Augmentation System"
      "AIS"
  , Acronym
      "LAHSO"
      "Land and Hold Short Operations"
      "AIS"
  , Acronym
      "LAME"
      "Licensed Aircraft Maintenance Engineer"
      ""
  , Acronym
      "LAN"
      "Inland"
      "AIS"
  , Acronym
      "LAT"
      "Latitude"
      "CASR Part 66"
  , Acronym
      "LDA"
      "Landing distance available"
      "AIS"
  , Acronym
      "LDG"
      "Landing"
      "AIS"
  , Acronym
      "LDI"
      "Landing Direction Indicator"
      "ICAO/AIP"
  , Acronym
      "LDP"
      "Landing decision point"
      "AIS"
  , Acronym
      "LEN"
      "Length"
      "AIS"
  , Acronym
      "LF"
      "Low Frequency (30 to 300 KHZ)"
      "ICAO"
  , Acronym
      "LGT"
      "Light, Lighting"
      "AIS"
  , Acronym
      "LGTD"
      "Lighted"
      "AIS"
  , Acronym
      "LIH"
      "Light Intensity High"
      "AIS"
  , Acronym
      "LIL"
      "Light Intensity Low"
      "AIS"
  , Acronym
      "LIM"
      "Light Intensity Medium"
      "AIS"
  , Acronym
      "LIOL"
      "Low Intensity Obstacle Lights"
      "AIS"
  , Acronym
      "LIRL"
      "Low Intensity Runway Lights"
      "AIS"
  , Acronym
      "LJR"
      "Low Jet Route"
      "AIS"
  , Acronym
      "LL"
      "Lower Limit"
      "AIS"
  , Acronym
      "LLN"
      "Low-level Navigation (by the MIL)"
      "AIS"
  , Acronym
      "LLO"
      "Low Level Operations (by the MIL)"
      "AIS"
  , Acronym
      "LLZ"
      "Localizer"
      "AIS"
  , Acronym
      "LM"
      "Locator (middle)"
      "AIS"
  , Acronym
      "LMT"
      "Local mean time"
      "AIS"
  , Acronym
      "LO"
      "Locator (outer)"
      "AIS"
  , Acronym
      "LOC"
      "Locally, Location, Located, Local"
      "AIS"
  , Acronym
      "LOE"
      "Lane of Entry"
      "AIS"
  , Acronym
      "LONG"
      "Longitude"
      "AIS"
  , Acronym
      "LOSA"
      "Line Operations Safety Audit"
      "AIS"
  , Acronym
      "LPT"
      "Liquid Penetrant Test"
      "AIS"
  , Acronym
      "LRCS"
      "Long-range communication system"
      ""
  , Acronym
      "LRG"
      "Long Range"
      ""
  , Acronym
      "LRI"
      "Line Replaceable Item"
      "FAR"
  , Acronym
      "LRNS"
      "Long-range navigation system"
      "AIS"
  , Acronym
      "LRU"
      "Line Replaceable Unit"
      "WATOG"
  , Acronym
      "LSA"
      "Light Sport Aircraft"
      "FAR"
  , Acronym
      "LSALT"
      "Lowest safe altitude"
      "WATOG"
  , Acronym
      "LSd"
      "CASA's Legal Services Division"
      ""
  , Acronym
      "LTD"
      "Limited"
      ""
  , Acronym
      "LUL"
      "Lowest Usable Level"
      "CASA"
  , Acronym
      "LV"
      "Light and Variable (relating to wind)"
      "AIS"
  , Acronym
      "LVE"
      "Leave, Leaving"
      "AIS"
  , Acronym
      "LVL"
      "Level"
      "AIS"
  , Acronym
      "LYR"
      "Layer, Layered"
      "AIS"
  , Acronym
      "M"
      "Mach number (followed by figures ie .02 = Mach .02)"
      "AIS"
  , Acronym
      "M (m)"
      "Metres (preceded by figures)"
      "AIS"
  , Acronym
      "M/E"
      "Multi-engine"
      "AIS"
  , Acronym
      "MAAT"
      "Manual Authoring and Assessment Tool"
      "CASA"
  , Acronym
      "MAE"
      "Men and Equipment"
      ""
  , Acronym
      "MAG"
      "Magnetic"
      ""
  , Acronym
      "MAHWP"
      "Missed Approach Holding Way-point"
      "AIS"
  , Acronym
      "MAINT"
      "Maintenance"
      "AIS"
  , Acronym
      "MAN"
      "Manual"
      "AIS"
  , Acronym
      "MAP"
      "Aeronautical Maps and Charts"
      "AIS"
  , Acronym
      "MAP"
      "Manifold Air Pressure"
      "AIS"
  , Acronym
      "MAPT"
      "Missed Approach Point"
      "AIS"
  , Acronym
      "MAR"
      "At Sea"
      ""
  , Acronym
      "MAUW"
      "Maximum All Up Weight"
      "AIS"
  , Acronym
      "MAWP"
      "Missed Approach Waypoint"
      "AIS"
  , Acronym
      "MAX"
      "Maximum"
      ""
  , Acronym
      "MBST"
      "Microburst"
      "AIS"
  , Acronym
      "MBZ"
      "Mandatory Broadcast Zone"
      "AIS"
  , Acronym
      "MCC"
      "Multi-crew Co-ordination"
      "AIP"
  , Acronym
      "MCM"
      "Maintenance control manual"
      "CASA"
  , Acronym
      "MCQFS"
      "Manual of criteria for the qualification of flight simulators               ICAO Doc 9625-AN/938"
      ""
  , Acronym
      "MCUR"
      "Mean Cycles Between Unscheduled Removals"
      "CASR Part 145"
  , Acronym
      "MCW"
      "Modulated Continuous Wave"
      "CASR Part 60"
  , Acronym
      "MDA"
      "Minimum Descent Altitude"
      "WATOG"
  , Acronym
      "MDF"
      "Medium Frequency Direction Finding Station"
      "AIS"
  , Acronym
      "MDR"
      "Major Defect Report"
      "AIS"
  , Acronym
      "MEA"
      "Minimum En-route Altitude"
      "AIS"
  , Acronym
      "MED"
      "Medical"
      "CAR 1988"
  , Acronym
      "MEL"
      "Minimum equipment list"
      "AIS"
  , Acronym
      "MERSITAB"
      "Manufacturing, Engineering and Related Services Industry Training               Advisory Body"
      "AIS"
  , Acronym
      "MET"
      "Meteorological, Meteorology"
      "ICAO"
  , Acronym
      "MET"
      "REPORT Aviation routine weather report"
      "CASR Part 66"
  , Acronym
      "METAR"
      "Aviation routine weather report (in aeronautical meteorological               code)"
      "AIS"
  , Acronym
      "METRAD"
      "MET Radar"
      "AIS"
  , Acronym
      "MF"
      "Medium Frequency (300 to 3000 KHZ)"
      "AIS"
  , Acronym
      "MHZ (mhz)"
      "Megahertz"
      "AIS"
  , Acronym
      "MIFG"
      "Shallow Fog"
      "AIS"
  , Acronym
      "MIL"
      "Military"
      "AIS"
  , Acronym
      "MIL-STD"
      "Military Standard"
      "AIS"
  , Acronym
      "MIN"
      "Minutes"
      "AIS"
  , Acronym
      "MIOL"
      "Medium Intensity Obstacle Lights"
      ""
  , Acronym
      "MIRL"
      "Medium Intensity Runway Lights"
      "AIS"
  , Acronym
      "MISC"
      "Miscellaneous"
      "AIS"
  , Acronym
      "MKR"
      "Marker Radio Beacon"
      "AIS"
  , Acronym
      "MLJ"
      "Military Low Jet"
      "AIS"
  , Acronym
      "MLJR"
      "Military Low Jet Route"
      "AIS"
  , Acronym
      "MLS"
      "Microwave Landing System"
      "AIS"
  , Acronym
      "MLW"
      "Maximum Landing Weight"
      "AIS"
  , Acronym
      "MM"
      "Middle Marker"
      "AIS"
  , Acronym
      "MMEL"
      "Master minimum equipment list"
      "AIS"
  , Acronym
      "MNM"
      "Minimum"
      "AIS"
  , Acronym
      "MNT"
      "Monitor, Monitoring,"
      "ICAO"
  , Acronym
      "MNTN"
      "Maintain, Maintained, Maintaining"
      "AIS"
  , Acronym
      "MOA"
      "Military Operating Area"
      "AIS"
  , Acronym
      "MOC"
      "Minimum obstacle clearance (required)"
      "AIS"
  , Acronym
      "MOD"
      "Moderate, Moderately"
      "AIS"
  , Acronym
      "MOD"
      "Moderate (used to indicate the intensity of WX phenomena, interference or static reports, eg MOD RA = moderate rain)."
      "ICAO/AIP"
  , Acronym
      "MON"
      "Above Mountains"
      "AIS"
  , Acronym
      "MOPS"
      "Minimum Operational Performance Standards"
      "AIS"
  , Acronym
      "MOS"
      "Manual of Standards"
      "AIS"
  , Acronym
      "MOU"
      "Memorandum of understanding"
      "AIS"
  , Acronym
      "MOV"
      "Move, Moved, Moving, Movement"
      "CAR 1998"
  , Acronym
      "MOWP"
      "Method of Working Plan"
      ""
  , Acronym
      "MPD"
      "Maintenance Planning Data"
      "AIS"
  , Acronym
      "MPI"
      "Magnetic Particle Inspection"
      "AIS"
  , Acronym
      "MPL"
      "Multi-crew Pilot Licence "
      "ATA/FAR"
  , Acronym
      "MPS"
      "Metres per Second"
      ""
  , Acronym
      "MPT"
      "Magnetic Particle Inspection Technician"
      "ICAO Annex 1 'Personnel Licensing'"
  , Acronym
      "MRB"
      "Maintenance Review Board"
      "AIS"
  , Acronym
      "MRBR"
      "Maintenance Review Board Report"
      ""
  , Acronym
      "MRG"
      "Medium Range"
      "ATA/FAR"
  , Acronym
      "MRO"
      "Maintenance, repair and overhaul"
      ""
  , Acronym
      "MRP"
      "ATS/MET Reporting Point"
      "AIS"
  , Acronym
      "MS"
      "Minus"
      "CASR Part 145"
  , Acronym
      "MSA"
      "Minimum sector altitude"
      "AIS"
  , Acronym
      "MSAS"
      "Multi-function Transport Satellite (MTSAT) Satellite-based Augmentation System"
      "AIS"
  , Acronym
      "MSDS"
      "Material Safety Data Sheets"
      "AIP"
  , Acronym
      "MSG"
      "Message"
      ""
  , Acronym
      "MSG"
      "Maintenance Steering Group"
      ""
  , Acronym
      "MSG-3"
      "Maintenance Steering Group Logic Process"
      "AIS"
  , Acronym
      "MSI"
      "Maintenance Significant Item"
      "ATA/FAR"
  , Acronym
      "MSL"
      "Mean Sea Level"
      ""
  , Acronym
      "MSOS"
      "Maintenance specific operation specification"
      "WATOG"
  , Acronym
      "MSSR"
      "Monopulse Secondary Surveillance Radar"
      "AIS"
  , Acronym
      "MT"
      "Mountain"
      "CASR Part 145"
  , Acronym
      "MTBF"
      "Mean Time Between Failure"
      "AIS"
  , Acronym
      "MTBR"
      "Mean Time Between Removals"
      "AIS"
  , Acronym
      "MTBUR"
      "Mean Time Between Unscheduled Removals"
      "WATOG"
  , Acronym
      "MTO"
      "Maintenance training organisation"
      "WATOG"
  , Acronym
      "MTOW"
      "Maximum take-off weight"
      "WATOG"
  , Acronym
      "MTP"
      "Maximum Tyre Pressure"
      "CASR Part 147"
  , Acronym
      "MTSAT"
      "Multi-function Transport Satellite"
      "AIP"
  , Acronym
      "MTTF"
      "Mean Time To Failure"
      "AIS"
  , Acronym
      "MTTR"
      "Mean Time To Repair"
      ""
  , Acronym
      "MTTUR"
      "Mean Time To Unscheduled Removal"
      "WATOG"
  , Acronym
      "MTUR"
      "Special Term For Data Transmittal"
      "WATOG"
  , Acronym
      "MTW"
      "Mountain waves"
      "WATOG"
  , Acronym
      "MVA"
      "Minimum Vector Altitude"
      "WATOG"
  , Acronym
      "MWO"
      "Meteorological Watch Office"
      "AIS"
  , Acronym
      "MX"
      "Mixed type of ice formation (white and clear)"
      "AIS"
  , Acronym
      "N"
      "North, North Latitude"
      "AIS"
  , Acronym
      "N1"
      "Gas Generator Speed"
      "AIS"
  , Acronym
      "N2"
      "Second Stage Turbine Speed"
      "AIS"
  , Acronym
      "NAA"
      "National Airworthiness Authority (for a country other than Australia)"
      ""
  , Acronym
      "NAD"
      "Non-acqueous Developer"
      ""
  , Acronym
      "NAIPS"
      "National aeronautical information processing system"
      "CAR 1998"
  , Acronym
      "NANDTB"
      "National Aerospace Non-destructive Testing Board"
      ""
  , Acronym
      "NANU"
      "Notice Advisory to NavStar Users"
      "AIP"
  , Acronym
      "NAP"
      "Noise Abatement Procedures"
      ""
  , Acronym
      "NAS"
      "National Airspace System"
      ""
  , Acronym
      "NASC"
      "National AIS System Centre"
      "AIS"
  , Acronym
      "NAT"
      "NAVAID Training"
      "AIS"
  , Acronym
      "NAV"
      "Navigation"
      ""
  , Acronym
      "NAVAID"
      "Navigation Aid"
      "AIS"
  , Acronym
      "NAWD"
      "Non-acqueous Wet Developer"
      "AIS"
  , Acronym
      "NB"
      "Northbound"
      "AIS"
  , Acronym
      "NBFR"
      "Not Before"
      ""
  , Acronym
      "NC"
      "No Change"
      "AIS"
  , Acronym
      "NDB"
      "Non-directional Radio Beacon"
      "AIS"
  , Acronym
      "NDI"
      "Non-destructive inspection"
      "AIS"
  , Acronym
      "NDT"
      "Non-destructive testing"
      "AIS"
  , Acronym
      "NE"
      "Northeast"
      "AIS"
  , Acronym
      "NEG"
      "Negative, No. Permission not granted or, That is not correct"
      "CAR 1988"
  , Acronym
      "NFRM"
      "Notice of Final Rule Making"
      "AIS"
  , Acronym
      "NG"
      "Gas Generator Speed"
      "AIS"
  , Acronym
      "NGT"
      "Night"
      "CASA"
  , Acronym
      "NIL"
      "None"
      ""
  , Acronym
      "NM"
      "Nautical Miles"
      "AIS"
  , Acronym
      "NML"
      "Normal"
      "AIS"
  , Acronym
      "NNE"
      "North North-East"
      "AIS"
  , Acronym
      "NNW"
      "North North-West"
      "AIS"
  , Acronym
      "NOF"
      "International NOTAM Office"
      "AIS"
  , Acronym
      "NOSIG"
      "No Significant Change"
      "AIS"
  , Acronym
      "NOTAM"
      "Notice To Airmen"
      "AIS"
  , Acronym
      "NOZ"
      "Normal Operating Zone"
      "AIP"
  , Acronym
      "NP"
      "Propeller Speed"
      "AIS"
  , Acronym
      "NPA"
      "Non-precision approach (procedures at aerodromes)"
      "AIS"
  , Acronym
      "NPC"
      "Notice of proposed change (consultation for amendment to a MOS)"
      ""
  , Acronym
      "NPRM"
      "Notice of proposed rule making"
      "AIS"
  , Acronym
      "NSC"
      "Nil Significant Cloud"
      "CAR 1998"
  , Acronym
      "NTA"
      "No TAF Amendment"
      "CASA"
  , Acronym
      "NTL"
      "National"
      "AIS"
  , Acronym
      "NTS"
      "Negative Torque Sensing System"
      "AIS"
  , Acronym
      "NTSB"
      "National Transportation Safety Board (of the USA)"
      "AIS"
  , Acronym
      "NTZ"
      "No-transgression zone"
      ""
  , Acronym
      "NVD"
      "Night Vision Device"
      ""
  , Acronym
      "NVED"
      "Night Vision Enhancement Devices"
      "AIP/ICAO"
  , Acronym
      "NVET"
      "National vocational education and training"
      ""
  , Acronym
      "NVFR"
      "Night Visual Flight Rules"
      ""
  , Acronym
      "NVG"
      "Night Vision Goggles (by the MIL)"
      "CASR Part 66"
  , Acronym
      "NVIS"
      "Night Vision Imaging Systems"
      ""
  , Acronym
      "NW"
      "North-West"
      "AIS"
  , Acronym
      "NXT"
      "Next"
      ""
  , Acronym
      "NZ"
      "New Zealand"
      "AIS"
  , Acronym
      "NZ CAA"
      "New Zealand Civil Aviation Authority"
      "AIS"
  , Acronym
      "NZS"
      "New Zealand Standard"
      ""
  , Acronym
      "O/R"
      "On Request"
      ""
  , Acronym
      "OAR"
      "Office of Airspace Regulation"
      ""
  , Acronym
      "OAT"
      "Outside Air Transport"
      ""
  , Acronym
      "OAT"
      "Outside Air Temperature"
      "CASA"
  , Acronym
      "OBPR"
      "Office of Best Practice Regulation"
      ""
  , Acronym
      "OBPR"
      "Office of Best Practice Regulation"
      ""
  , Acronym
      "OBS"
      "Omni Bearing Selector"
      ""
  , Acronym
      "OBS"
      "Observe, Observed, Observation"
      ""
  , Acronym
      "OBSC"
      "Obscure, Obscured, Obscuring"
      ""
  , Acronym
      "OBST"
      "Obstacle"
      ""
  , Acronym
      "OBSTR"
      "Obstruction"
      ""
  , Acronym
      "OC"
      "Operating Certificate"
      ""
  , Acronym
      "OCA"
      "Obstacle Clearing Altitude"
      ""
  , Acronym
      "OCA"
      "Oceanic Control Area"
      ""
  , Acronym
      "OCC"
      "Occulting (light)"
      ""
  , Acronym
      "OCH"
      "Obstacle Clearance Height"
      ""
  , Acronym
      "OCNL"
      "Occasional, Occasionally"
      ""
  , Acronym
      "OCTA"
      "Outside Control Area"
      ""
  , Acronym
      "OCTR"
      "Outside Control Zone"
      ""
  , Acronym
      "OEI"
      "One Engine Inoperative"
      "AIS"
  , Acronym
      "OEM"
      "Original Equipment Manufacturer"
      ""
  , Acronym
      "OFZ"
      "Obstacle Free Zone"
      ""
  , Acronym
      "OHD"
      "Overhead"
      ""
  , Acronym
      "OHS"
      "Occupation Health and Safety"
      ""
  , Acronym
      "OHSMS"
      "Occupational Health and Safety Management Systems"
      ""
  , Acronym
      "OIS"
      "Obstacle Identification Surface"
      ""
  , Acronym
      "OLC"
      "Office of Legal Counsel"
      ""
  , Acronym
      "OLDI"
      "On-line Data Interchange"
      ""
  , Acronym
      "OLS"
      "Obstacle Limitation Surface"
      ""
  , Acronym
      "OM"
      "Operations Manual"
      ""
  , Acronym
      "OM"
      "Outer Marker"
      ""
  , Acronym
      "OPA"
      "Opaque, white type of ice formation"
      ""
  , Acronym
      "OPC"
      "Office of Parliamentary Counsel"
      ""
  , Acronym
      "OPMET"
      "Operational Meteorological"
      ""
  , Acronym
      "OPN"
      "Operational Notification Message (message type designator)"
      ""
  , Acronym
      "OPN"
      "Open, Opening, Opened"
      ""
  , Acronym
      "OPR"
      "Operator, Operate, Operative, Operating, Operational"
      ""
  , Acronym
      "OPS"
      "Operations"
      ""
  , Acronym
      "OSIP"
      "Overhaul and Special Inspections Periods"
      ""
  , Acronym
      "OT"
      "Other Times"
      "CASA"
  , Acronym
      "OTAC"
      "Overseas Territories Aviation Circulars "
      "Air Safety Support International (UK) "
  , Acronym
      "OTAR"
      "Overseas Territories Aviation Requirements"
      "Air Safety Support International (UK) "
  , Acronym
      "OTLK"
      "Outlook (used in Sigmet messages for volcanic ash and tropical cyclones)"
      "Air Safety Support International (UK)"
  , Acronym
      "OTP"
      "On Top"
      "Air Safety Support International (UK)"
  , Acronym
      "OUBD"
      "Outboard"
      ""
  , Acronym
      "OVC"
      "Overcast"
      ""
  , Acronym
      "OW"
      "Over Water"
      ""
  , Acronym
      "P.."
      "Prohibited Area (followed by identification)"
      ""
  , Acronym
      "PA"
      "Precision Approach"
      ""
  , Acronym
      "PAL"
      "Pilot Activated Lighting"
      "AIS"
  , Acronym
      "PANS"
      "Procedures for Air Navigation Services"
      ""
  , Acronym
      "PANS-OPS"
      "Procedures for Air Navigation Services - Aircraft Operations ICAO Doc 8168 Vol I and II"
      "AIS"
  , Acronym
      "PAPI"
      "Precision Approach Path Indicator"
      "AIS"
  , Acronym
      "PAR"
      "Precision Approach Radar"
      "ICAO"
  , Acronym
      "PARL"
      "Parallel"
      "AIS"
  , Acronym
      "PATC"
      "Precision Approach Terrain Chart (followed by name/title)"
      "AIS"
  , Acronym
      "PAX"
      "Passengers"
      "AIS"
  , Acronym
      "PBM"
      "Power Plant Build-Up Manual"
      "AIS"
  , Acronym
      "PC"
      "Personal Computer"
      "AIS"
  , Acronym
      "PCATD"
      "Personal Computer-based Aviation Training Device"
      "WATOG"
  , Acronym
      "PCD"
      "Proceed, Proceeding"
      ""
  , Acronym
      "PCL"
      "Pilot Controlled Lighting"
      ""
  , Acronym
      "PCN"
      "Pavement classification number"
      "AIS"
  , Acronym
      "PCT"
      "Practical consolidation training"
      "AIS"
  , Acronym
      "PDC"
      "Pre-Departure Clearance"
      "ICAO/AIP"
  , Acronym
      "PDO"
      "Product distribution organisation"
      "CASR Part 66"
  , Acronym
      "PDOP"
      "Positional Dilution of Precision"
      "AIS"
  , Acronym
      "PDSE"
      "Pre-departure Service Check "
      "CASR Part 144"
  , Acronym
      "PEC"
      "Pressure Error Correction"
      ""
  , Acronym
      "PERM"
      "Permanent"
      ""
  , Acronym
      "PFC"
      "Porous Friction Course"
      "AIS"
  , Acronym
      "PFR"
      "Preferred Route"
      "AIS"
  , Acronym
      "PH"
      "Public Holiday"
      "AIS"
  , Acronym
      "PIB"
      "Pre-flight information bulletin"
      "AIS"
  , Acronym
      "PIC"
      "Pilot-in-command"
      "AIS"
  , Acronym
      "PICUS"
      "Pilot-in-Command Under Supervision"
      "ICAO/AIP"
  , Acronym
      "PIFR"
      "Private IFR rating"
      "CAR 1988"
  , Acronym
      "PILS"
      "Practice ILS"
      ""
  , Acronym
      "PIREP"
      "Pilot Reports"
      "CAR 1988"
  , Acronym
      "PJE"
      "Parachute Jumping Exercise"
      "AIS"
  , Acronym
      "PL"
      "Ice Pellets"
      "WATOG"
  , Acronym
      "PLN"
      "Flight Plan"
      "AIS"
  , Acronym
      "PLVL"
      "Present Level"
      "CASA"
  , Acronym
      "PMP"
      "Primary Maintenance Process"
      "AIS"
  , Acronym
      "PN"
      "Prior Notice Required"
      "AIS"
  , Acronym
      "PNR"
      "Point of No Return"
      "WATOG"
  , Acronym
      "PO"
      "Dust Devils"
      "AIS"
  , Acronym
      "POB"
      "Persons on Board"
      "AIS"
  , Acronym
      "POH"
      "Pilot Operating Handbook"
      ""
  , Acronym
      "POSS"
      "Possible"
      "AIS"
  , Acronym
      "PPI"
      "Plan Position Indicator"
      ""
  , Acronym
      "PPL"
      "Private Pilot Licence"
      "AIS"
  , Acronym
      "PPR"
      "Prior Permission Required"
      "AIS"
  , Acronym
      "PPS"
      "Precise Positioning Service"
      "CAR 1988"
  , Acronym
      "PPSN"
      "Present Position"
      "AIS"
  , Acronym
      "PRD"
      "Prohibited, Restricted and Danger Areas"
      ""
  , Acronym
      "PRFG"
      "Aerodrome Partially Covered by Fog (MET code)"
      "AIS"
  , Acronym
      "PRI"
      "Primary"
      "AIS"
  , Acronym
      "PRKG"
      "Parking"
      "AIS"
  , Acronym
      "PRM"
      "Precision Runway Monitoring"
      "AIS"
  , Acronym
      "PRM"
      "Precision runway monitoring"
      "AIS"
  , Acronym
      "PROB"
      "Probable, Probability"
      "AIS"
  , Acronym
      "PROC"
      "Procedure"
      "AIP"
  , Acronym
      "PROV"
      "Provisional"
      "AIS"
  , Acronym
      "PS"
      "Plus"
      "AIS"
  , Acronym
      "PSG"
      "Passing"
      "AIS"
  , Acronym
      "PSN"
      "Position"
      "AIS"
  , Acronym
      "PSP"
      "Pierced Steel Plank"
      "AIS"
  , Acronym
      "PSR"
      "Primary Surveillance Radar"
      "AIS"
  , Acronym
      "PSYS"
      "Pressure System(s)"
      "AIS"
  , Acronym
      "PTBL"
      "Portable"
      "AIS"
  , Acronym
      "PTN"
      "Procedure Turn"
      "AIS"
  , Acronym
      "PTT"
      "Push To Talk"
      "AIS"
  , Acronym
      "PVT"
      "Private"
      "AIS"
  , Acronym
      "PWR"
      "Power"
      ""
  , Acronym
      "QDM"
      "Magnetic Heading (zero wind)"
      "AIS"
  , Acronym
      "QDR"
      "Magnetic Bearing"
      "AIS"
  , Acronym
      "QEC"
      "Quick Engine Change Unit"
      "AIP"
  , Acronym
      "QFE"
      "An altimeter set to QFE will read zero when the aircraft is on the runway"
      "AIP"
  , Acronym
      "QMS"
      "Quality Management System"
      "WATOG"
  , Acronym
      "QNH"
      "Altimeter subscale setting to obtain elevation or altitude"
      ""
  , Acronym
      "QTE"
      "True Bearing"
      ""
  , Acronym
      "QTG"
      "Qualification test guide"
      "AIS"
  , Acronym
      "QUAD"
      "Quadrant"
      "AIP"
  , Acronym
      "R"
      "Red"
      ""
  , Acronym
      "R"
      "Right (runway system identification)"
      "AIS"
  , Acronym
      "R...."
      "Restricted Area (followed by number)"
      "AIS"
  , Acronym
      "R/T"
      "Radio Telephone"
      "AIS"
  , Acronym
      "RA"
      "Rain"
      "AIS"
  , Acronym
      "RA-Aus"
      "Recreational Aviation Australia "
      ""
  , Acronym
      "RAAA"
      "Recreational Aviation Association of Australia "
      "AIS"
  , Acronym
      "RAAO"
      "Recreational Aviation Administrative Organisation"
      ""
  , Acronym
      "RAC"
      "Rules of the Air and Air Traffic Services"
      ""
  , Acronym
      "RAD"
      "Radius"
      ""
  , Acronym
      "RAeS"
      "Royal Aeronautical Society"
      "AIS"
  , Acronym
      "RAFC"
      "Regional Area Forecast Centre"
      "AIS"
  , Acronym
      "RAG"
      "Ragged"
      ""
  , Acronym
      "RAG"
      "Runway Arresting Gear"
      "AIS"
  , Acronym
      "RAI"
      "Runway Alignment Indicator"
      "AIS"
  , Acronym
      "RAIM"
      "Receiver Autonomous Integrity Monitoring"
      "AIS"
  , Acronym
      "RAM"
      "Repair and Maintenance"
      "AIS"
  , Acronym
      "RAPAC"
      "Regional Airspace Users Advisory Committee"
      ""
  , Acronym
      "RAPIC"
      "Radar Picture (MET)"
      ""
  , Acronym
      "RAS"
      "Radar Advisory Service"
      ""
  , Acronym
      "RASF"
      "Regional Aviation Safety Forum"
      ""
  , Acronym
      "RAT"
      "RAM Air Turbine"
      "AIS"
  , Acronym
      "RCA"
      "Reach Cruising Altitude,"
      "AIS"
  , Acronym
      "RCA"
      "Request For Corrective Action"
      ""
  , Acronym
      "RCC"
      "Rescue Coordination Centre"
      "AIS"
  , Acronym
      "RCH"
      "Reach, Reaching"
      "AIS"
  , Acronym
      "RCL"
      "Runway Centre Line"
      "AIS"
  , Acronym
      "RCLL"
      "Runway Centre Line Lights"
      "AIS"
  , Acronym
      "RCLM"
      "Runway Centre Line Marking"
      "AIS"
  , Acronym
      "RDL"
      "Radial"
      "AIS"
  , Acronym
      "RDO"
      "Radio"
      "AIS"
  , Acronym
      "RE..."
      "Recent (used to qualify weather phenomena, eg RERA = recent rain)"
      "CASA"
  , Acronym
      "REC"
      "Receive, Receiver, Received"
      "AIS"
  , Acronym
      "REDL"
      "Runway Edge Lights"
      "AIS"
  , Acronym
      "REF"
      "Reference to... Refer to..."
      "AIS"
  , Acronym
      "REG"
      "Registration"
      "AIS"
  , Acronym
      "RENL"
      "Runway End Lights"
      "AIS"
  , Acronym
      "REP"
      "Report, Reported, Reporting, Reporting Point"
      "AIS"
  , Acronym
      "REQ"
      "Request, Requested"
      "AIS"
  , Acronym
      "RERTE"
      "Re-route"
      "AIS"
  , Acronym
      "RES"
      "Reserve Fuel"
      "AIS"
  , Acronym
      "RESA"
      "Runway end safety area"
      "AIS"
  , Acronym
      "RESTR"
      "Restrictions"
      "AIS"
  , Acronym
      "REV"
      "Review"
      "ICAO/AIP"
  , Acronym
      "RFACA"
      "Royal Federation of Aero Clubs of Australia"
      "AIS"
  , Acronym
      "RFC"
      "Request for change"
      "AIS"
  , Acronym
      "RFC"
      "Regional Forecasting Centre (Met)"
      ""
  , Acronym
      "RFFS"
      "Rescue and Fire Fighting Services"
      ""
  , Acronym
      "RH"
      "Radio Height"
      "AIS"
  , Acronym
      "RHC"
      "Right-hand Circuit"
      "CASA"
  , Acronym
      "RIF"
      "Reclearance in Flight"
      "AIS"
  , Acronym
      "RIFTO"
      "Restricted Instrument Flight Take-off"
      "AIS"
  , Acronym
      "RIS"
      "Regulation Impact Statement"
      "OBPR"
  , Acronym
      "RIS"
      "Radar information service"
      ""
  , Acronym
      "RL"
      "Report Leaving"
      "ORR"
  , Acronym
      "RLA"
      "Relay to"
      "AIP/ICAO"
  , Acronym
      "RLLS"
      "Runway Lead-in Lighting"
      "AIS"
  , Acronym
      "RMIT"
      "Royal Melbourne Institute of Technology"
      "AIS"
  , Acronym
      "RMK"
      "Remark(s)"
      "AIS"
  , Acronym
      "RMS"
      "Root Mean Square"
      ""
  , Acronym
      "RNAV"
      "Area navigation"
      "AIS"
  , Acronym
      "RNP"
      "Required navigation performance"
      ""
  , Acronym
      "RO"
      "Registered operator"
      "AIP"
  , Acronym
      "ROBEX"
      "Regional OPMET Bulletin Exchanges"
      ""
  , Acronym
      "ROC"
      "Rate of Climb"
      "CASR Part 43"
  , Acronym
      "ROD"
      "Rate of Descent"
      "AIS"
  , Acronym
      "ROFOR"
      "Route Forecast (in aeronautical meteorological code)"
      "AIS"
  , Acronym
      "RPA"
      "Rules and practices for aerodromes"
      "AIS"
  , Acronym
      "RPI"
      "Radar Position Indicator"
      "AIS"
  , Acronym
      "RPI"
      "Regulatory Performance Indicator"
      "CAR 1988"
  , Acronym
      "RPI"
      "Runway Point of Intercept"
      "AIS"
  , Acronym
      "RPL"
      "Recognition of prior learning"
      "CAR 1988"
  , Acronym
      "RPM"
      "Revolutions Per Minute"
      "AIS"
  , Acronym
      "RPT"
      "Regular public transport"
      "CASR Part 66"
  , Acronym
      "RQ"
      "Require(d)"
      ""
  , Acronym
      "RQMNTS"
      "Requirements"
      "CAR 1988"
  , Acronym
      "RR"
      "Report Reaching"
      "AIS"
  , Acronym
      "RRM"
      "Routine Reportable Matters"
      "AIS"
  , Acronym
      "RSC"
      "Rescue Sub-Centre"
      "CASA"
  , Acronym
      "RSCD"
      "Runway Surface Condition"
      "CASA"
  , Acronym
      "RSP"
      "Responder Beacon"
      "AIS"
  , Acronym
      "RSR"
      "En route Surveillance Radar"
      "AIS"
  , Acronym
      "RTB"
      "Return to Base"
      "AIS"
  , Acronym
      "RTCA"
      "Radio Technical Commission for Aeronautics"
      "CASA"
  , Acronym
      "RTE"
      "Route"
      ""
  , Acronym
      "RTF"
      "Radio Telephone"
      ""
  , Acronym
      "RTHL"
      "Runway Threshold Light(s)"
      "AIS"
  , Acronym
      "RTIL"
      "Runway Threshold Identification Lights"
      "AIS"
  , Acronym
      "RTN"
      "Return, Returned, Returning"
      "AIS"
  , Acronym
      "RTO"
      "Rejected take-off"
      "AIS"
  , Acronym
      "RTO"
      "Registered training organisation"
      "AIS"
  , Acronym
      "RTS"
      "Return to Service"
      "AIP"
  , Acronym
      "RTZL"
      "Runway Touchdown Zone Light(s)"
      "CASR Part 147"
  , Acronym
      "RVR"
      "Runway visual range"
      "AIS"
  , Acronym
      "RVSM"
      "Reduced Vertical Separation Minimum"
      "AIS"
  , Acronym
      "RWS"
      "Runway Strip"
      "AIP/ICAO"
  , Acronym
      "RWY"
      "Runway"
      "ICAO/CAR 1988"
  , Acronym
      "S"
      "South, South Latitude"
      "AIS"
  , Acronym
      "S-I"
      "Straight-In"
      "AIS"
  , Acronym
      "S/E"
      "Single-engine"
      "AIP"
  , Acronym
      "SA"
      "Sand"
      "WATOG"
  , Acronym
      "SA"
      "Selective Availability"
      ""
  , Acronym
      "SA"
      "Situation Awareness"
      "AIS"
  , Acronym
      "SAAA"
      "Sport Aircraft Association of Australia"
      ""
  , Acronym
      "SAL"
      "Supplementary Airline Licence"
      ""
  , Acronym
      "SALR"
      "Saturated Adiabatic Lapse Rate"
      ""
  , Acronym
      "SALS"
      "Simple Approach Lighting System"
      "AIS"
  , Acronym
      "SAR"
      "Search and Rescue"
      ""
  , Acronym
      "SARPs"
      "Standards and Recommended Practices "
      "ICAO"
  , Acronym
      "SARTIME"
      "Time search action required"
      "AIS"
  , Acronym
      "SARWATCH"
      "Search and Rescue Watch"
      "ICAO"
  , Acronym
      "SATCOM"
      "Satellite Communication"
      ""
  , Acronym
      "SB"
      "Service Bulletin"
      ""
  , Acronym
      "SB"
      "Southbound"
      "AIS"
  , Acronym
      "SBAS"
      "Satellite Based Augmentation System"
      "AIS"
  , Acronym
      "SC"
      "Stratocumulus"
      "AIS"
  , Acronym
      "SCC"
      "Standards Consultative Committee"
      ""
  , Acronym
      "SCP"
      "Safety Critical Personnel"
      "AIS"
  , Acronym
      "SCT"
      "Scattered (meteorological)"
      "CASA"
  , Acronym
      "SDBY"
      "Standby"
      ""
  , Acronym
      "SDC"
      "Standard Departure Clearance"
      "AIS"
  , Acronym
      "SDR"
      "Service difficulty report"
      "AIS"
  , Acronym
      "SE"
      "South East"
      "AIS"
  , Acronym
      "SEA"
      "Sea (used in connection with sea-surface temperature and state               of the sea)"
      "CAR 1998"
  , Acronym
      "SEACO"
      "Single engine aircraft only"
      "AIS"
  , Acronym
      "SEC"
      "Seconds"
      "AIS"
  , Acronym
      "SECN"
      "Section"
      ""
  , Acronym
      "SECT"
      "Sector"
      "AIS"
  , Acronym
      "SELCAL"
      "Selective Calling System"
      "AIS"
  , Acronym
      "SER"
      "Service, Servicing, Served"
      "AIS"
  , Acronym
      "SEV"
      "Severe (used eg to qualify icing and turbulence report)"
      "AIS"
  , Acronym
      "SFC"
      "Surface"
      "AIS"
  , Acronym
      "SFL"
      "Sequenced Flashing Lights"
      "AIS"
  , Acronym
      "SG"
      "Snow Grains"
      "AIS"
  , Acronym
      "SH..."
      "Showers (followed by RA=rain, SN=snow, PL=ice pellets, GR=hail,               GS=small hail and/or snow pellets or combinations thereof, eg, SHRASN               = showers of rain and snow)"
      "AIS"
  , Acronym
      "SHF"
      "Super High Frequency (3,000 to 30,000 MHZ)"
      "AIS"
  , Acronym
      "SHP"
      "Shaft Horse Power"
      "AIS"
  , Acronym
      "SID"
      "Standard instrument departure"
      "AIS"
  , Acronym
      "SIF"
      "Selective Identification"
      ""
  , Acronym
      "SIG"
      "Significant"
      "ICAO/AIP"
  , Acronym
      "SIGMET"
      "Information concerning en route weather phenomena which may affect               the safety of aircraft operations"
      "AIS"
  , Acronym
      "SIL"
      "Service Information Letter"
      "AIS"
  , Acronym
      "SIMUL"
      "Simultaneous, or Simultaneously"
      "AIS"
  , Acronym
      "SKC"
      "Sky Clear"
      ""
  , Acronym
      "SKED"
      "Schedule, Scheduled"
      "AIS"
  , Acronym
      "SL"
      "Service Letter"
      "AIS"
  , Acronym
      "SLP"
      "Speed Limiting Point"
      "AIS"
  , Acronym
      "SLW"
      "Slow, Slowly"
      ""
  , Acronym
      "SM"
      "Safety Manager"
      "AIS"
  , Acronym
      "SMC"
      "Surface Movement Control"
      "AIS"
  , Acronym
      "SMR"
      "Surface Movement Radar"
      ""
  , Acronym
      "SMS"
      "Safety management system"
      "AIS"
  , Acronym
      "SN"
      "Snow"
      "AIS"
  , Acronym
      "SNOWTAM"
      "A special series NOTAM notifying the presence or removal of hazardous               conditions due to snow, ice, slush or standing water associated               with snow, slush and ice on the movement area"
      "CAR 1998"
  , Acronym
      "SOC"
      "Start of Climb"
      "AIS"
  , Acronym
      "SOE"
      "Schedule of experience"
      "AIS"
  , Acronym
      "SOM (SoM)"
      "System of Maintenance"
      "AIS"
  , Acronym
      "SOPs"
      "Standard Operating Procedures"
      "CAR 1988"
  , Acronym
      "SOR"
      "Summary of responses "
      "CAR 1998"
  , Acronym
      "SOT"
      "Start of TORA (take-off)"
      ""
  , Acronym
      "SP"
      "Single Pilot"
      "CASA"
  , Acronym
      "SPA"
      "Sport Aviation"
      "AIS"
  , Acronym
      "SPECI"
      "Aviation Special Weather (in aeronautical meteorological code)"
      "AIS"
  , Acronym
      "SPFIB"
      "Specific Preflight Information Bulletin"
      "AIS"
  , Acronym
      "SPOT"
      "Spotwind"
      "AIS"
  , Acronym
      "SPS"
      "Standard Positioning Service"
      "AIS"
  , Acronym
      "SQ"
      "Squall"
      "AIS"
  , Acronym
      "SR"
      "Sunrise"
      ""
  , Acronym
      "SRB"
      "Safety Review Board"
      "AIS"
  , Acronym
      "SRD"
      "Standard Radar Departure"
      "AIS"
  , Acronym
      "SRG"
      "Short Range"
      ""
  , Acronym
      "SRM"
      "Structural Repair Manual"
      "AIS"
  , Acronym
      "SRR"
      "Search and Rescue Region"
      "AIS"
  , Acronym
      "SRY"
      "Secondary"
      "WATOG"
  , Acronym
      "SS"
      "Sandstorm"
      "AIS"
  , Acronym
      "SS"
      "Sunset"
      "AIS"
  , Acronym
      "SSB"
      "Single Sideband"
      "AIS"
  , Acronym
      "SSCRO"
      "Senate Standing Committee on Regulations and Ordinances"
      "AIS"
  , Acronym
      "SSE"
      "South South-East"
      "AIS"
  , Acronym
      "SSI"
      "Structural Significant Item"
      ""
  , Acronym
      "SSM"
      "System Schematics Manual"
      "AIS"
  , Acronym
      "SSR"
      "Secondary Surveillance Radar"
      "WATOG"
  , Acronym
      "SST"
      "Supersonic Transport"
      "WATOG"
  , Acronym
      "SSW"
      "South South-West"
      "AIS"
  , Acronym
      "ST"
      "Stratus"
      "AIS"
  , Acronym
      "STA"
      "Straight in Approach"
      "AIS"
  , Acronym
      "STAR"
      "Standard Arrival Route"
      "AIS"
  , Acronym
      "STAR"
      "Standard instrumental arrival"
      "AIS"
  , Acronym
      "STARs"
      "Standard terminal arrival routes"
      "AIP"
  , Acronym
      "STC"
      "Supplemental Type Certificate"
      "ICAO"
  , Acronym
      "STD"
      "Synthetic training device"
      ""
  , Acronym
      "STD"
      "Standard"
      "CASR Part 21"
  , Acronym
      "STF"
      "Stratiform"
      "CASR Part 60"
  , Acronym
      "STN"
      "Station"
      "AIS"
  , Acronym
      "STNR"
      "Stationary"
      "AIS"
  , Acronym
      "STODA"
      "Supplementary Take-off Distance"
      "AIS"
  , Acronym
      "STOL"
      "Short Take-off and Landing"
      "AIS"
  , Acronym
      "STS"
      "Status"
      "AIS"
  , Acronym
      "STWL"
      "Stopway Light(s)"
      "AIS"
  , Acronym
      "SUBJ"
      "Subject to"
      "AIS"
  , Acronym
      "SUP"
      "Supplement (AIP Supplement)"
      "AIS"
  , Acronym
      "SUPPS"
      "Regional Supplementary Procedures"
      "AIS"
  , Acronym
      "SVCBL"
      "Serviceable"
      "AIS"
  , Acronym
      "SVY"
      "Survey Operations"
      "AIS"
  , Acronym
      "SW"
      "South-West"
      "AIS"
  , Acronym
      "SWS"
      "Soft Wet Surface"
      "AIS"
  , Acronym
      "SWY"
      "Stopway"
      "AIS"
  , Acronym
      "T"
      "Temperature"
      "AIS"
  , Acronym
      "T-VASIS"
      "'T'Visual Approach Slope Indicator System (pronounced 'TEE-VAR-SIS')"
      "AIS"
  , Acronym
      "TA"
      "Transition Altitude"
      "AIS"
  , Acronym
      "TA"
      "Traffic advisory"
      "AIS"
  , Acronym
      "TAAATS"
      "The Australian Advanced Air Traffic System"
      "AIS"
  , Acronym
      "TAAM"
      "Airservices Australia's Total Airspace Airport and Aerodrome Modeller"
      "ICAO/AIP"
  , Acronym
      "TAC"
      "Terminal Area Chart"
      ""
  , Acronym
      "TACAN"
      "Tactical Air Navigation Aid"
      "AIS"
  , Acronym
      "TAF"
      "Aerodrome Forecast"
      "AIS"
  , Acronym
      "TAF"
      "Terminal Area Forecast"
      "AIS"
  , Acronym
      "TAIL"
      "Tailwind"
      "AIS"
  , Acronym
      "TAR"
      "Terminal Area Surveillance Area"
      ""
  , Acronym
      "TAS"
      "True Airspeed"
      "AIS"
  , Acronym
      "TAT"
      "Terminal Area Thunderstorm Service (meteorological term)"
      "AIS"
  , Acronym
      "TAWS"
      "Terrain awareness and warning system"
      "AIS"
  , Acronym
      "TAWS-B+ system"
      "A terrain awareness and warning system that is equipped with a visual display and complies with the requirements for Class B equipment expressed in TSO-C151, TSO-C151a or TSO-C151b."
      "AIS"
  , Acronym
      "TAX"
      "Taxiing, Taxi"
      "CAO 20.18"
  , Acronym
      "TBA"
      "To be advised"
      "CAO Part 20, Section 20.18, Issue 9"
  , Acronym
      "TBO"
      "Time Between Overhauls"
      "AIS"
  , Acronym
      "TC"
      "Type Certificate"
      "AIS"
  , Acronym
      "TC"
      "Tropical Cyclone"
      "WATOG"
  , Acronym
      "TCAC"
      "Tropical Cyclone Advisory Centre"
      "AIS"
  , Acronym
      "TCAS"
      "Traffic alert and collision avoidance system"
      "CASA"
  , Acronym
      "TCDS"
      "Type certificate data sheet"
      ""
  , Acronym
      "TCH"
      "Threshold Crossing Height"
      "ICAO"
  , Acronym
      "TCO"
      "Training and Checking Organisation"
      "CASR Part 21"
  , Acronym
      "TCTA"
      "Trans-continental Control Area"
      "AIS"
  , Acronym
      "TCU"
      "Towering Cumulus (met)"
      ""
  , Acronym
      "TDO"
      "Tornado"
      "AIS"
  , Acronym
      "TDZ"
      "Touchdown Zone"
      "AIS"
  , Acronym
      "TECR"
      "Technical Reason"
      "AIS"
  , Acronym
      "TEL"
      "Telephone"
      "AIS"
  , Acronym
      "TEM"
      "(Illustrated) Tool And Equipment Manual"
      "AIS"
  , Acronym
      "TEM"
      "Threat and Error Management"
      "AIS"
  , Acronym
      "TEMPO"
      "Temporary, Temporarily (meteorologically)"
      "WATOG"
  , Acronym
      "TERPs"
      "Terminal and En-route procedures"
      ""
  , Acronym
      "TFC"
      "Traffic"
      "AIS"
  , Acronym
      "TFR"
      "Terrain Following Radar (by the MIL)"
      "AIS"
  , Acronym
      "TGL"
      "Touch &amp; Go Landing"
      "AIS"
  , Acronym
      "TGS"
      "Taxiing Guidance System"
      "AIS"
  , Acronym
      "THR"
      "Threshold"
      "AIS"
  , Acronym
      "THRU"
      "Through"
      "AIS"
  , Acronym
      "TIBA"
      "Traffic Information Broadcasts by Aircraft"
      "AIS"
  , Acronym
      "TIL"
      "Until"
      "AIS"
  , Acronym
      "TIP"
      "Until Past (place)"
      "AIS"
  , Acronym
      "TIS"
      "Time in service"
      "AIS"
  , Acronym
      "TKOF"
      "Take-off"
      "AIS"
  , Acronym
      "TLOF"
      "Touchdown and lift-off area"
      ""
  , Acronym
      "TLS"
      "Transponder Landing System"
      "AIS"
  , Acronym
      "TLW"
      "Time Limited WIP (work in progress)"
      "ICAO"
  , Acronym
      "TMA"
      "Terminal Control Area"
      ""
  , Acronym
      "TMG"
      "Track Made Good"
      "AIS"
  , Acronym
      "TNA"
      "Training Needs Analysis"
      "AIS"
  , Acronym
      "TNA"
      "Turn Altitude"
      ""
  , Acronym
      "TNH"
      "Turn Height"
      ""
  , Acronym
      "TNS"
      "Transitional Surface"
      "AIS"
  , Acronym
      "TOC"
      "Top of Climb"
      "AIS"
  , Acronym
      "TODA"
      "Take-off distance available"
      "AIS"
  , Acronym
      "TOP"
      "Cloud Top"
      "AIS"
  , Acronym
      "TORA"
      "Take-off run available"
      "ICAO"
  , Acronym
      "TOT"
      "Turbine Outlet Temperature"
      "AIS"
  , Acronym
      "TP"
      "Turning Point"
      "ICAO"
  , Acronym
      "TR"
      "Track"
      ""
  , Acronym
      "TRA"
      "Temporary Reserved Airspace"
      "AIS"
  , Acronym
      "TRAN"
      "Transition"
      "AIS"
  , Acronym
      "TRANS"
      "Transmits, Transmitter"
      "AIS"
  , Acronym
      "Trike"
      "A manned, powered hang-glider"
      "AIS"
  , Acronym
      "TRL"
      "Transition Level"
      "AIS"
  , Acronym
      "TROP"
      "Tropopause"
      ""
  , Acronym
      "TS..."
      "Thunderstorm (followed by RA=rain, SN=snow, PE=ice pellets, GR=hail,               GS=small hail and/or snow pellets or combinations thereof, eg TSRASN               = thunderstorm with rain and snow)"
      "AIS"
  , Acronym
      "TSI"
      "Time Since Installation"
      "AIS"
  , Acronym
      "TSO"
      "Technical Standard Order"
      "AIS"
  , Acronym
      "TSO"
      "Time Since Overhaul"
      "WATOG"
  , Acronym
      "TTF"
      "Trend Type Forecast"
      ""
  , Acronym
      "TTMRA"
      "Trans Tasman Mutual Recognition Agreement"
      "WATOG"
  , Acronym
      "TURB"
      "Turbulence"
      ""
  , Acronym
      "TWR"
      "Aerodrome Control Tower, Aerodrome Control"
      "AIS"
  , Acronym
      "TWY"
      "Taxiway"
      "AIS"
  , Acronym
      "TWYL"
      "Taxiway Link"
      "AIS"
  , Acronym
      "TYP"
      "Type of Aircraft"
      "AIS"
  , Acronym
      "TYPH"
      "Typhoon"
      "AIS"
  , Acronym
      "U/S"
      "Unserviceable"
      "AIS"
  , Acronym
      "UAB"
      "Until Advised By"
      "AIS"
  , Acronym
      "UAV"
      "Unmanned aerial vehicles"
      "AIS"
  , Acronym
      "UDF"
      "UHF Direction Finding Stations"
      "AIS"
  , Acronym
      "UFIT"
      "Uncontrolled Flight Into Terrain"
      "CASR Part 101"
  , Acronym
      "UFN"
      "Until Further Notice"
      "AIS"
  , Acronym
      "UHDT"
      "Unable Higher Due Traffic"
      ""
  , Acronym
      "UHF"
      "Ultra High Frequency (300 to 3,000 MHZ)"
      "AIS"
  , Acronym
      "UIR"
      "Upper Flight Information Region"
      "AIS"
  , Acronym
      "UK"
      "United Kingdom"
      "AIS"
  , Acronym
      "UL"
      "Upper Limits"
      "AIS"
  , Acronym
      "UNA"
      "Unable"
      ""
  , Acronym
      "UNAP"
      "Unable to Approve"
      "AIS"
  , Acronym
      "UNICOM"
      "Universal Communications"
      "AIS"
  , Acronym
      "UNL"
      "Unlimited"
      "AIS"
  , Acronym
      "UNLC"
      "Unlicensed"
      ""
  , Acronym
      "UNREL"
      "Unreliable"
      "AIS"
  , Acronym
      "US FAR"
      "Federal Aviation Regulations (of the USA)"
      "AIS"
  , Acronym
      "USA"
      "United States of America"
      ""
  , Acronym
      "UTA"
      "Upper Control Area"
      ""
  , Acronym
      "UTC"
      "Coordinated Universal Time"
      ""
  , Acronym
      "V"
      "Velocity"
      "AIS"
  , Acronym
      "V1"
      "Take-off Decision Speed"
      "AIS"
  , Acronym
      "V2"
      "Take-off Safety Speed (Applicable to Larger Multi-engine Aircraft)"
      ""
  , Acronym
      "VA"
      "Volcanic Ash"
      ""
  , Acronym
      "VA"
      "Maximum Manoeuvring Speed"
      ""
  , Acronym
      "VAA-H"
      "Visual Approach Area - Helicopter"
      "AIS"
  , Acronym
      "VAAC"
      "Volcanic Ash Advisory Centre"
      ""
  , Acronym
      "VAC"
      "Visual Approach Chart (followed by name/title)"
      ""
  , Acronym
      "VAL"
      "In Valleys"
      ""
  , Acronym
      "VAR"
      "Magnetic Variation"
      "AIS"
  , Acronym
      "VASIS"
      "Visual Approach Slope Indicator System"
      "AIS"
  , Acronym
      "VC"
      "Vicinity of the aerodrome (followed by FG=fog, FC=funnel cloud,               PO=dust/sand whirls, BLDU=blowing dust, BLSA=blowing sand or BLSN=blowing               snow; eg, VCFG=vicinity fog"
      "AIS"
  , Acronym
      "VCY"
      "Vicinity"
      "AIS"
  , Acronym
      "VDF"
      "VHF Direction Finding Station"
      "AIS"
  , Acronym
      "VEC"
      "Visual En-route Chart"
      "AIS"
  , Acronym
      "VER"
      "Vertical"
      "AIS"
  , Acronym
      "VFE"
      "Flap Extension Speed"
      "AIS"
  , Acronym
      "VFR"
      "Visual flight rules"
      "AIS"
  , Acronym
      "VHF"
      "Very High Frequency (30 to 300 MHZ)"
      ""
  , Acronym
      "VHFCOM"
      "Very High Frequency Communications"
      ""
  , Acronym
      "VIA"
      "By way of..."
      "AIS"
  , Acronym
      "VIMD"
      "Maximum Drag Speed"
      ""
  , Acronym
      "VIP"
      "Very Important Person"
      "AIS"
  , Acronym
      "VIS"
      "Visibility"
      ""
  , Acronym
      "VLE"
      "Maximum Speed with Landing Gear Extended"
      "AIS"
  , Acronym
      "VLF"
      "Very Low Frequency (3 to 30 KHZ)"
      "AIS"
  , Acronym
      "VLJ"
      "Very Light Jet"
      ""
  , Acronym
      "VLO"
      "Landing Gear Operating"
      "AIS"
  , Acronym
      "VLO2"
      "Landing Gear Operation Down"
      ""
  , Acronym
      "VLR"
      "Very Long Range"
      ""
  , Acronym
      "VMAN"
      "Visual Manoeuvre Speed"
      ""
  , Acronym
      "VMC"
      "Visual Meteorological Conditions"
      "AIS"
  , Acronym
      "VMC"
      "Minimum Control Speed"
      ""
  , Acronym
      "VMCA"
      "Minimum Control Airspeed Airborne (Red line speed)"
      "AIS"
  , Acronym
      "VMCG"
      "Minimum Control Airspeed on the Ground"
      ""
  , Acronym
      "VMO"
      "Maximum Structural Cruising Speed"
      ""
  , Acronym
      "VNAV"
      "Vertical Navigation"
      ""
  , Acronym
      "VNE"
      "Never Exceed Speed"
      ""
  , Acronym
      "VNO"
      "Normal Operating Speed"
      "ICAO"
  , Acronym
      "VOLMET"
      "Meteorological Information for Aircraft in Flight"
      "ICAO"
  , Acronym
      "VOR"
      "VHF Omni-directional Radio Range (OMNI)"
      ""
  , Acronym
      "VP"
      "Variable Pitch Propellers"
      "AIS"
  , Acronym
      "VR"
      "The speed at which the aircraft is rotated to lift off from the runway"
      "AIS"
  , Acronym
      "VRB"
      "Variable"
      ""
  , Acronym
      "VSI"
      "Vertical Speed Indicator"
      ""
  , Acronym
      "VSI"
      "Clean Stall Speed"
      "AIS"
  , Acronym
      "VSL"
      "Vertical speed limit"
      ""
  , Acronym
      "VSO"
      "Stalling speed, minimum steady flight speed in the landing configuration"
      ""
  , Acronym
      "VSO"
      "Stall speed with undercarriage and flap selected"
      "ICAO"
  , Acronym
      "VSSE"
      "Safe single-engine Speed"
      "CAR 1998"
  , Acronym
      "VTC"
      "Visual Terminal Chart"
      ""
  , Acronym
      "VTOL"
      "Vertical take-off and landing"
      ""
  , Acronym
      "VTOSS"
      "Take-off Safely Speed"
      "AIS"
  , Acronym
      "VX"
      "Best Angle of Climb Speed"
      "ICAO"
  , Acronym
      "VXSE"
      "Best Single-engine Angle of Climb Speed"
      ""
  , Acronym
      "Vy"
      "Best Rate of Climb Speed"
      ""
  , Acronym
      "VYSE"
      "Best Single-engine Rate of Climb Speed (Blue line speed)"
      ""
  , Acronym
      "W"
      "Watt"
      ""
  , Acronym
      "W"
      "West, West Longitude"
      ""
  , Acronym
      "W"
      "White"
      ""
  , Acronym
      "W/V"
      "Wind Velocity"
      "AIS"
  , Acronym
      "WAAS"
      "Wide Area Augmentation System"
      "AIS"
  , Acronym
      "WAC"
      "World Aeronautical Chart  ICAO 1:1,000,000 (followed by name/title)"
      ""
  , Acronym
      "WAFC"
      "World Area Forecast Centre"
      ""
  , Acronym
      "WAFS"
      "World Area Forecast System"
      "AIS"
  , Acronym
      "WATIR"
      "Weather and Terminal Information Reciter"
      "AIS"
  , Acronym
      "WATOG"
      "World Airlines Technical Operations Glossary"
      ""
  , Acronym
      "WB"
      "Westbound"
      "AIS"
  , Acronym
      "WBM"
      "Weight And Balance Manual"
      ""
  , Acronym
      "WDI"
      "Wind Direction Indicator"
      "AIS"
  , Acronym
      "WDSPR"
      "Widespread"
      "WATOG"
  , Acronym
      "WED"
      "Wednesday"
      "AIS"
  , Acronym
      "WEF"
      "With Effect From, Effective From"
      "AIS"
  , Acronym
      "WGS-84"
      "World Geodetic System 1984"
      "AIS"
  , Acronym
      "WI"
      "Within"
      "AIS"
  , Acronym
      "WID"
      "Width"
      "AIS"
  , Acronym
      "WIE"
      "With Immediate Effect, Effective Immediately"
      "AIS"
  , Acronym
      "WILCO"
      "Will Comply"
      "AIS"
  , Acronym
      "WIND"
      "Wind (used in connection with direction and speed) WINTEM Forecast               upper wind and temperature at specified points (in aeronautical               met code)"
      "AIS"
  , Acronym
      "WIP"
      "Work in Progress"
      "AIS"
  , Acronym
      "WKN"
      "Weaken, Weakening"
      "AIS"
  , Acronym
      "WM"
      "Wiring Manual"
      "AIS"
  , Acronym
      "WMO"
      "World Meteorological Organisation"
      "AIS"
  , Acronym
      "WNW"
      "West North-West"
      "WATOG"
  , Acronym
      "WO"
      "Without"
      ""
  , Acronym
      "WPT"
      "Way-point"
      "AIS"
  , Acronym
      "WRNG"
      "Warning"
      "AIS"
  , Acronym
      "WS"
      "Wind Shear"
      "AIS"
  , Acronym
      "WSW"
      "West South-West"
      "AIS"
  , Acronym
      "WT"
      "Weight"
      "AIS"
  , Acronym
      "WTSPT"
      "Water Spout"
      "AIS"
  , Acronym
      "WWW"
      "World Wide Web"
      "AIS"
  , Acronym
      "WX"
      "Weather"
      "AIS"
  , Acronym
      "X"
      "Cross"
      "AIS"
  , Acronym
      "XBAR"
      "Crossbar (of approach lighting system)"
      "AIS"
  , Acronym
      "XNG"
      "Crossing"
      "AIS"
  , Acronym
      "XS"
      "Atmospherics"
      "AIS"
  , Acronym
      "XSEC"
      "Extra Section"
      "AIS"
  , Acronym
      "Y"
      "Yellow"
      "AIS"
  , Acronym
      "YCZ"
      "Yellow Caution Zone"
      "WATOG"
  , Acronym
      "YR"
      "Your/s"
      "AIS"
  , Acronym
      "Z"
      "Coordinated Universal Time (in meteorological messages)"
      "AIS"
  ]
