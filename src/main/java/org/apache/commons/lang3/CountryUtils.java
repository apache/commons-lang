package org.apache.commons.lang3;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Lightweight country lookup utility for Apache Commons Lang.
 *
 * Provides static, unmodifiable mappings for:
 * - ISO 3166-1 alpha-2 (input)
 * - ISO 3166-1 alpha-3
 * - ISO 3166 numeric code
 * - Common short country name
 * - International calling (phone) code (as string including '+' prefix where applicable)
 *
 * Data is embedded as static Java maps (no external dependencies or resource loading).
 *
 * NOTE: This is intended to be a convenience utility. For authoritative or frequently-updated
 * datasets consider loading from a maintained resource or official dataset. Small differences
 * in naming or telephone-code representation may exist depending on sources.
 */
public class CountryUtils {

    private CountryUtils(){

    }
    private static final Map<String, String> ISO2_TO_ISO3;
    private static final Map<String, String> ISO2_TO_NUMERIC;
    private static final Map<String, String> ISO2_TO_NAME;
    private static final Map<String, String> ISO2_TO_PHONE;

    static {
        Map<String, String> iso3 = new HashMap<>();
        Map<String, String> numeric = new HashMap<>();
        Map<String, String> name = new HashMap<>();
        Map<String, String> phone = new HashMap<>();

        // Data rows: ISO2, ISO3, numeric, name, phone code
        // Best-effort comprehensive list (approx. 240+ entries).
        // If you want to update a single entry, update both maps consistently.

        // Format examples:
        // iso3.put("US", "USA"); numeric.put("US", "840"); name.put("US", "United States"); phone.put("US", "+1");

        iso3.put("AF", "AFG"); numeric.put("AF", "004"); name.put("AF", "Afghanistan"); phone.put("AF", "+93");
        iso3.put("AL", "ALB"); numeric.put("AL", "008"); name.put("AL", "Albania"); phone.put("AL", "+355");
        iso3.put("DZ", "DZA"); numeric.put("DZ", "012"); name.put("DZ", "Algeria"); phone.put("DZ", "+213");
        iso3.put("AS", "ASM"); numeric.put("AS", "016"); name.put("AS", "American Samoa"); phone.put("AS", "+1-684");
        iso3.put("AD", "AND"); numeric.put("AD", "020"); name.put("AD", "Andorra"); phone.put("AD", "+376");
        iso3.put("AO", "AGO"); numeric.put("AO", "024"); name.put("AO", "Angola"); phone.put("AO", "+244");
        iso3.put("AI", "AIA"); numeric.put("AI", "660"); name.put("AI", "Anguilla"); phone.put("AI", "+1-264");
        iso3.put("AQ", "ATA"); numeric.put("AQ", "010"); name.put("AQ", "Antarctica"); phone.put("AQ", "+672");
        iso3.put("AG", "ATG"); numeric.put("AG", "028"); name.put("AG", "Antigua and Barbuda"); phone.put("AG", "+1-268");
        iso3.put("AR", "ARG"); numeric.put("AR", "032"); name.put("AR", "Argentina"); phone.put("AR", "+54");
        iso3.put("AM", "ARM"); numeric.put("AM", "051"); name.put("AM", "Armenia"); phone.put("AM", "+374");
        iso3.put("AW", "ABW"); numeric.put("AW", "533"); name.put("AW", "Aruba"); phone.put("AW", "+297");
        iso3.put("AU", "AUS"); numeric.put("AU", "036"); name.put("AU", "Australia"); phone.put("AU", "+61");
        iso3.put("AT", "AUT"); numeric.put("AT", "040"); name.put("AT", "Austria"); phone.put("AT", "+43");
        iso3.put("AZ", "AZE"); numeric.put("AZ", "031"); name.put("AZ", "Azerbaijan"); phone.put("AZ", "+994");
        iso3.put("BS", "BHS"); numeric.put("BS", "044"); name.put("BS", "Bahamas"); phone.put("BS", "+1-242");
        iso3.put("BH", "BHR"); numeric.put("BH", "048"); name.put("BH", "Bahrain"); phone.put("BH", "+973");
        iso3.put("BD", "BGD"); numeric.put("BD", "050"); name.put("BD", "Bangladesh"); phone.put("BD", "+880");
        iso3.put("BB", "BRB"); numeric.put("BB", "052"); name.put("BB", "Barbados"); phone.put("BB", "+1-246");
        iso3.put("BY", "BLR"); numeric.put("BY", "112"); name.put("BY", "Belarus"); phone.put("BY", "+375");
        iso3.put("BE", "BEL"); numeric.put("BE", "056"); name.put("BE", "Belgium"); phone.put("BE", "+32");
        iso3.put("BZ", "BLZ"); numeric.put("BZ", "084"); name.put("BZ", "Belize"); phone.put("BZ", "+501");
        iso3.put("BJ", "BEN"); numeric.put("BJ", "204"); name.put("BJ", "Benin"); phone.put("BJ", "+229");
        iso3.put("BM", "BMU"); numeric.put("BM", "060"); name.put("BM", "Bermuda"); phone.put("BM", "+1-441");
        iso3.put("BT", "BTN"); numeric.put("BT", "064"); name.put("BT", "Bhutan"); phone.put("BT", "+975");
        iso3.put("BO", "BOL"); numeric.put("BO", "068"); name.put("BO", "Bolivia (Plurinational State of)"); phone.put("BO", "+591");
        iso3.put("BQ", "BES"); numeric.put("BQ", "535"); name.put("BQ", "Bonaire, Sint Eustatius and Saba"); phone.put("BQ", "+599");
        iso3.put("BA", "BIH"); numeric.put("BA", "070"); name.put("BA", "Bosnia and Herzegovina"); phone.put("BA", "+387");
        iso3.put("BW", "BWA"); numeric.put("BW", "072"); name.put("BW", "Botswana"); phone.put("BW", "+267");
        iso3.put("BV", "BVT"); numeric.put("BV", "074"); name.put("BV", "Bouvet Island"); phone.put("BV", "+47");
        iso3.put("BR", "BRA"); numeric.put("BR", "076"); name.put("BR", "Brazil"); phone.put("BR", "+55");
        iso3.put("IO", "IOT"); numeric.put("IO", "086"); name.put("IO", "British Indian Ocean Territory"); phone.put("IO", "+246");
        iso3.put("BN", "BRN"); numeric.put("BN", "096"); name.put("BN", "Brunei Darussalam"); phone.put("BN", "+673");
        iso3.put("BG", "BGR"); numeric.put("BG", "100"); name.put("BG", "Bulgaria"); phone.put("BG", "+359");
        iso3.put("BF", "BFA"); numeric.put("BF", "854"); name.put("BF", "Burkina Faso"); phone.put("BF", "+226");
        iso3.put("BI", "BDI"); numeric.put("BI", "108"); name.put("BI", "Burundi"); phone.put("BI", "+257");
        iso3.put("CV", "CPV"); numeric.put("CV", "132"); name.put("CV", "Cabo Verde"); phone.put("CV", "+238");
        iso3.put("KH", "KHM"); numeric.put("KH", "116"); name.put("KH", "Cambodia"); phone.put("KH", "+855");
        iso3.put("CM", "CMR"); numeric.put("CM", "120"); name.put("CM", "Cameroon"); phone.put("CM", "+237");
        iso3.put("CA", "CAN"); numeric.put("CA", "124"); name.put("CA", "Canada"); phone.put("CA", "+1");
        iso3.put("KY", "CYM"); numeric.put("KY", "136"); name.put("KY", "Cayman Islands"); phone.put("KY", "+1-345");
        iso3.put("CF", "CAF"); numeric.put("CF", "140"); name.put("CF", "Central African Republic"); phone.put("CF", "+236");
        iso3.put("TD", "TCD"); numeric.put("TD", "148"); name.put("TD", "Chad"); phone.put("TD", "+235");
        iso3.put("CL", "CHL"); numeric.put("CL", "152"); name.put("CL", "Chile"); phone.put("CL", "+56");
        iso3.put("CN", "CHN"); numeric.put("CN", "156"); name.put("CN", "China"); phone.put("CN", "+86");
        iso3.put("CX", "CXR"); numeric.put("CX", "162"); name.put("CX", "Christmas Island"); phone.put("CX", "+61");
        iso3.put("CC", "CCK"); numeric.put("CC", "166"); name.put("CC", "Cocos (Keeling) Islands"); phone.put("CC", "+61");
        iso3.put("CO", "COL"); numeric.put("CO", "170"); name.put("CO", "Colombia"); phone.put("CO", "+57");
        iso3.put("KM", "COM"); numeric.put("KM", "174"); name.put("KM", "Comoros"); phone.put("KM", "+269");
        iso3.put("CG", "COG"); numeric.put("CG", "178"); name.put("CG", "Congo"); phone.put("CG", "+242");
        iso3.put("CD", "COD"); numeric.put("CD", "180"); name.put("CD", "Congo (Democratic Republic of the)"); phone.put("CD", "+243");
        iso3.put("CK", "COK"); numeric.put("CK", "184"); name.put("CK", "Cook Islands"); phone.put("CK", "+682");
        iso3.put("CR", "CRI"); numeric.put("CR", "188"); name.put("CR", "Costa Rica"); phone.put("CR", "+506");
        iso3.put("CI", "CIV"); numeric.put("CI", "384"); name.put("CI", "Côte d'Ivoire"); phone.put("CI", "+225");
        iso3.put("HR", "HRV"); numeric.put("HR", "191"); name.put("HR", "Croatia"); phone.put("HR", "+385");
        iso3.put("CU", "CUB"); numeric.put("CU", "192"); name.put("CU", "Cuba"); phone.put("CU", "+53");
        iso3.put("CW", "CUW"); numeric.put("CW", "531"); name.put("CW", "Curaçao"); phone.put("CW", "+599");
        iso3.put("CY", "CYP"); numeric.put("CY", "196"); name.put("CY", "Cyprus"); phone.put("CY", "+357");
        iso3.put("CZ", "CZE"); numeric.put("CZ", "203"); name.put("CZ", "Czechia"); phone.put("CZ", "+420");
        iso3.put("DK", "DNK"); numeric.put("DK", "208"); name.put("DK", "Denmark"); phone.put("DK", "+45");
        iso3.put("DJ", "DJI"); numeric.put("DJ", "262"); name.put("DJ", "Djibouti"); phone.put("DJ", "+253");
        iso3.put("DM", "DMA"); numeric.put("DM", "212"); name.put("DM", "Dominica"); phone.put("DM", "+1-767");
        iso3.put("DO", "DOM"); numeric.put("DO", "214"); name.put("DO", "Dominican Republic"); phone.put("DO", "+1-809, +1-829, +1-849");
        iso3.put("EC", "ECU"); numeric.put("EC", "218"); name.put("EC", "Ecuador"); phone.put("EC", "+593");
        iso3.put("EG", "EGY"); numeric.put("EG", "818"); name.put("EG", "Egypt"); phone.put("EG", "+20");
        iso3.put("SV", "SLV"); numeric.put("SV", "222"); name.put("SV", "El Salvador"); phone.put("SV", "+503");
        iso3.put("GQ", "GNQ"); numeric.put("GQ", "226"); name.put("GQ", "Equatorial Guinea"); phone.put("GQ", "+240");
        iso3.put("ER", "ERI"); numeric.put("ER", "232"); name.put("ER", "Eritrea"); phone.put("ER", "+291");
        iso3.put("EE", "EST"); numeric.put("EE", "233"); name.put("EE", "Estonia"); phone.put("EE", "+372");
        iso3.put("SZ", "SWZ"); numeric.put("SZ", "748"); name.put("SZ", "Eswatini"); phone.put("SZ", "+268");
        iso3.put("ET", "ETH"); numeric.put("ET", "231"); name.put("ET", "Ethiopia"); phone.put("ET", "+251");
        iso3.put("FK", "FLK"); numeric.put("FK", "238"); name.put("FK", "Falkland Islands (Malvinas)"); phone.put("FK", "+500");
        iso3.put("FO", "FRO"); numeric.put("FO", "234"); name.put("FO", "Faroe Islands"); phone.put("FO", "+298");
        iso3.put("FJ", "FJI"); numeric.put("FJ", "242"); name.put("FJ", "Fiji"); phone.put("FJ", "+679");
        iso3.put("FI", "FIN"); numeric.put("FI", "246"); name.put("FI", "Finland"); phone.put("FI", "+358");
        iso3.put("FR", "FRA"); numeric.put("FR", "250"); name.put("FR", "France"); phone.put("FR", "+33");
        iso3.put("GF", "GUF"); numeric.put("GF", "254"); name.put("GF", "French Guiana"); phone.put("GF", "+594");
        iso3.put("PF", "PYF"); numeric.put("PF", "258"); name.put("PF", "French Polynesia"); phone.put("PF", "+689");
        iso3.put("TF", "ATF"); numeric.put("TF", "260"); name.put("TF", "French Southern Territories"); phone.put("TF", "+262");
        iso3.put("GA", "GAB"); numeric.put("GA", "266"); name.put("GA", "Gabon"); phone.put("GA", "+241");
        iso3.put("GM", "GMB"); numeric.put("GM", "270"); name.put("GM", "Gambia"); phone.put("GM", "+220");
        iso3.put("GE", "GEO"); numeric.put("GE", "268"); name.put("GE", "Georgia"); phone.put("GE", "+995");
        iso3.put("DE", "DEU"); numeric.put("DE", "276"); name.put("DE", "Germany"); phone.put("DE", "+49");
        iso3.put("GH", "GHA"); numeric.put("GH", "288"); name.put("GH", "Ghana"); phone.put("GH", "+233");
        iso3.put("GI", "GIB"); numeric.put("GI", "292"); name.put("GI", "Gibraltar"); phone.put("GI", "+350");
        iso3.put("GR", "GRC"); numeric.put("GR", "300"); name.put("GR", "Greece"); phone.put("GR", "+30");
        iso3.put("GL", "GRL"); numeric.put("GL", "304"); name.put("GL", "Greenland"); phone.put("GL", "+299");
        iso3.put("GD", "GRD"); numeric.put("GD", "308"); name.put("GD", "Grenada"); phone.put("GD", "+1-473");
        iso3.put("GP", "GLP"); numeric.put("GP", "312"); name.put("GP", "Guadeloupe"); phone.put("GP", "+590");
        iso3.put("GU", "GUM"); numeric.put("GU", "316"); name.put("GU", "Guam"); phone.put("GU", "+1-671");
        iso3.put("GT", "GTM"); numeric.put("GT", "320"); name.put("GT", "Guatemala"); phone.put("GT", "+502");
        iso3.put("GG", "GGY"); numeric.put("GG", "831"); name.put("GG", "Guernsey"); phone.put("GG", "+44-1481");
        iso3.put("GN", "GIN"); numeric.put("GN", "324"); name.put("GN", "Guinea"); phone.put("GN", "+224");
        iso3.put("GW", "GNB"); numeric.put("GW", "624"); name.put("GW", "Guinea-Bissau"); phone.put("GW", "+245");
        iso3.put("GY", "GUY"); numeric.put("GY", "328"); name.put("GY", "Guyana"); phone.put("GY", "+592");
        iso3.put("HT", "HTI"); numeric.put("HT", "332"); name.put("HT", "Haiti"); phone.put("HT", "+509");
        iso3.put("HM", "HMD"); numeric.put("HM", "334"); name.put("HM", "Heard Island and McDonald Islands"); phone.put("HM", "+672");
        iso3.put("VA", "VAT"); numeric.put("VA", "336"); name.put("VA", "Holy See"); phone.put("VA", "+379");
        iso3.put("HN", "HND"); numeric.put("HN", "340"); name.put("HN", "Honduras"); phone.put("HN", "+504");
        iso3.put("HK", "HKG"); numeric.put("HK", "344"); name.put("HK", "Hong Kong"); phone.put("HK", "+852");
        iso3.put("HU", "HUN"); numeric.put("HU", "348"); name.put("HU", "Hungary"); phone.put("HU", "+36");
        iso3.put("IS", "ISL"); numeric.put("IS", "352"); name.put("IS", "Iceland"); phone.put("IS", "+354");
        iso3.put("IN", "IND"); numeric.put("IN", "356"); name.put("IN", "India"); phone.put("IN", "+91");
        iso3.put("ID", "IDN"); numeric.put("ID", "360"); name.put("ID", "Indonesia"); phone.put("ID", "+62");
        iso3.put("IR", "IRN"); numeric.put("IR", "364"); name.put("IR", "Iran (Islamic Republic of)"); phone.put("IR", "+98");
        iso3.put("IQ", "IRQ"); numeric.put("IQ", "368"); name.put("IQ", "Iraq"); phone.put("IQ", "+964");
        iso3.put("IE", "IRL"); numeric.put("IE", "372"); name.put("IE", "Ireland"); phone.put("IE", "+353");
        iso3.put("IM", "IMN"); numeric.put("IM", "833"); name.put("IM", "Isle of Man"); phone.put("IM", "+44-1624");
        iso3.put("IL", "ISR"); numeric.put("IL", "376"); name.put("IL", "Israel"); phone.put("IL", "+972");
        iso3.put("IT", "ITA"); numeric.put("IT", "380"); name.put("IT", "Italy"); phone.put("IT", "+39");
        iso3.put("JM", "JAM"); numeric.put("JM", "388"); name.put("JM", "Jamaica"); phone.put("JM", "+1-876");
        iso3.put("JP", "JPN"); numeric.put("JP", "392"); name.put("JP", "Japan"); phone.put("JP", "+81");
        iso3.put("JE", "JEY"); numeric.put("JE", "832"); name.put("JE", "Jersey"); phone.put("JE", "+44-1534");
        iso3.put("JO", "JOR"); numeric.put("JO", "400"); name.put("JO", "Jordan"); phone.put("JO", "+962");
        iso3.put("KZ", "KAZ"); numeric.put("KZ", "398"); name.put("KZ", "Kazakhstan"); phone.put("KZ", "+7");
        iso3.put("KE", "KEN"); numeric.put("KE", "404"); name.put("KE", "Kenya"); phone.put("KE", "+254");
        iso3.put("KI", "KIR"); numeric.put("KI", "296"); name.put("KI", "Kiribati"); phone.put("KI", "+686");
        iso3.put("KP", "PRK"); numeric.put("KP", "408"); name.put("KP", "Korea (Democratic People's Republic of)"); phone.put("KP", "+850");
        iso3.put("KR", "KOR"); numeric.put("KR", "410"); name.put("KR", "Korea, Republic of"); phone.put("KR", "+82");
        iso3.put("KW", "KWT"); numeric.put("KW", "414"); name.put("KW", "Kuwait"); phone.put("KW", "+965");
        iso3.put("KG", "KGZ"); numeric.put("KG", "417"); name.put("KG", "Kyrgyzstan"); phone.put("KG", "+996");
        iso3.put("LA", "LAO"); numeric.put("LA", "418"); name.put("LA", "Lao People's Democratic Republic"); phone.put("LA", "+856");
        iso3.put("LV", "LVA"); numeric.put("LV", "428"); name.put("LV", "Latvia"); phone.put("LV", "+371");
        iso3.put("LB", "LBN"); numeric.put("LB", "422"); name.put("LB", "Lebanon"); phone.put("LB", "+961");
        iso3.put("LS", "LSO"); numeric.put("LS", "426"); name.put("LS", "Lesotho"); phone.put("LS", "+266");
        iso3.put("LR", "LBR"); numeric.put("LR", "430"); name.put("LR", "Liberia"); phone.put("LR", "+231");
        iso3.put("LY", "LBY"); numeric.put("LY", "434"); name.put("LY", "Libya"); phone.put("LY", "+218");
        iso3.put("LI", "LIE"); numeric.put("LI", "438"); name.put("LI", "Liechtenstein"); phone.put("LI", "+423");
        iso3.put("LT", "LTU"); numeric.put("LT", "440"); name.put("LT", "Lithuania"); phone.put("LT", "+370");
        iso3.put("LU", "LUX"); numeric.put("LU", "442"); name.put("LU", "Luxembourg"); phone.put("LU", "+352");
        iso3.put("MO", "MAC"); numeric.put("MO", "446"); name.put("MO", "Macao"); phone.put("MO", "+853");
        iso3.put("MG", "MDG"); numeric.put("MG", "450"); name.put("MG", "Madagascar"); phone.put("MG", "+261");
        iso3.put("MW", "MWI"); numeric.put("MW", "454"); name.put("MW", "Malawi"); phone.put("MW", "+265");
        iso3.put("MY", "MYS"); numeric.put("MY", "458"); name.put("MY", "Malaysia"); phone.put("MY", "+60");
        iso3.put("MV", "MDV"); numeric.put("MV", "462"); name.put("MV", "Maldives"); phone.put("MV", "+960");
        iso3.put("ML", "MLI"); numeric.put("ML", "466"); name.put("ML", "Mali"); phone.put("ML", "+223");
        iso3.put("MT", "MLT"); numeric.put("MT", "470"); name.put("MT", "Malta"); phone.put("MT", "+356");
        iso3.put("MH", "MHL"); numeric.put("MH", "584"); name.put("MH", "Marshall Islands"); phone.put("MH", "+692");
        iso3.put("MQ", "MTQ"); numeric.put("MQ", "474"); name.put("MQ", "Martinique"); phone.put("MQ", "+596");
        iso3.put("MR", "MRT"); numeric.put("MR", "478"); name.put("MR", "Mauritania"); phone.put("MR", "+222");
        iso3.put("MU", "MUS"); numeric.put("MU", "480"); name.put("MU", "Mauritius"); phone.put("MU", "+230");
        iso3.put("YT", "MYT"); numeric.put("YT", "175"); name.put("YT", "Mayotte"); phone.put("YT", "+262");
        iso3.put("MX", "MEX"); numeric.put("MX", "484"); name.put("MX", "Mexico"); phone.put("MX", "+52");
        iso3.put("FM", "FSM"); numeric.put("FM", "583"); name.put("FM", "Micronesia (Federated States of)"); phone.put("FM", "+691");
        iso3.put("MD", "MDA"); numeric.put("MD", "498"); name.put("MD", "Moldova, Republic of"); phone.put("MD", "+373");
        iso3.put("MC", "MCO"); numeric.put("MC", "492"); name.put("MC", "Monaco"); phone.put("MC", "+377");
        iso3.put("MN", "MNG"); numeric.put("MN", "496"); name.put("MN", "Mongolia"); phone.put("MN", "+976");
        iso3.put("ME", "MNE"); numeric.put("ME", "499"); name.put("ME", "Montenegro"); phone.put("ME", "+382");
        iso3.put("MS", "MSR"); numeric.put("MS", "500"); name.put("MS", "Montserrat"); phone.put("MS", "+1-664");
        iso3.put("MA", "MAR"); numeric.put("MA", "504"); name.put("MA", "Morocco"); phone.put("MA", "+212");
        iso3.put("MZ", "MOZ"); numeric.put("MZ", "508"); name.put("MZ", "Mozambique"); phone.put("MZ", "+258");
        iso3.put("MM", "MMR"); numeric.put("MM", "104"); name.put("MM", "Myanmar"); phone.put("MM", "+95");
        iso3.put("NA", "NAM"); numeric.put("NA", "516"); name.put("NA", "Namibia"); phone.put("NA", "+264");
        iso3.put("NR", "NRU"); numeric.put("NR", "520"); name.put("NR", "Nauru"); phone.put("NR", "+674");
        iso3.put("NP", "NPL"); numeric.put("NP", "524"); name.put("NP", "Nepal"); phone.put("NP", "+977");
        iso3.put("NL", "NLD"); numeric.put("NL", "528"); name.put("NL", "Netherlands"); phone.put("NL", "+31");
        iso3.put("NC", "NCL"); numeric.put("NC", "540"); name.put("NC", "New Caledonia"); phone.put("NC", "+687");
        iso3.put("NZ", "NZL"); numeric.put("NZ", "554"); name.put("NZ", "New Zealand"); phone.put("NZ", "+64");
        iso3.put("NI", "NIC"); numeric.put("NI", "558"); name.put("NI", "Nicaragua"); phone.put("NI", "+505");
        iso3.put("NE", "NER"); numeric.put("NE", "562"); name.put("NE", "Niger"); phone.put("NE", "+227");
        iso3.put("NG", "NGA"); numeric.put("NG", "566"); name.put("NG", "Nigeria"); phone.put("NG", "+234");
        iso3.put("NU", "NIU"); numeric.put("NU", "570"); name.put("NU", "Niue"); phone.put("NU", "+683");
        iso3.put("NF", "NFK"); numeric.put("NF", "574"); name.put("NF", "Norfolk Island"); phone.put("NF", "+672");
        iso3.put("MK", "MKD"); numeric.put("MK", "807"); name.put("MK", "North Macedonia"); phone.put("MK", "+389");
        iso3.put("MP", "MNP"); numeric.put("MP", "580"); name.put("MP", "Northern Mariana Islands"); phone.put("MP", "+1-670");
        iso3.put("NO", "NOR"); numeric.put("NO", "578"); name.put("NO", "Norway"); phone.put("NO", "+47");
        iso3.put("OM", "OMN"); numeric.put("OM", "512"); name.put("OM", "Oman"); phone.put("OM", "+968");
        iso3.put("PK", "PAK"); numeric.put("PK", "586"); name.put("PK", "Pakistan"); phone.put("PK", "+92");
        iso3.put("PW", "PLW"); numeric.put("PW", "585"); name.put("PW", "Palau"); phone.put("PW", "+680");
        iso3.put("PS", "PSE"); numeric.put("PS", "275"); name.put("PS", "Palestine, State of"); phone.put("PS", "+970");
        iso3.put("PA", "PAN"); numeric.put("PA", "591"); name.put("PA", "Panama"); phone.put("PA", "+507");
        iso3.put("PG", "PNG"); numeric.put("PG", "598"); name.put("PG", "Papua New Guinea"); phone.put("PG", "+675");
        iso3.put("PY", "PRY"); numeric.put("PY", "600"); name.put("PY", "Paraguay"); phone.put("PY", "+595");
        iso3.put("PE", "PER"); numeric.put("PE", "604"); name.put("PE", "Peru"); phone.put("PE", "+51");
        iso3.put("PH", "PHL"); numeric.put("PH", "608"); name.put("PH", "Philippines"); phone.put("PH", "+63");
        iso3.put("PN", "PCN"); numeric.put("PN", "612"); name.put("PN", "Pitcairn"); phone.put("PN", "+870");
        iso3.put("PL", "POL"); numeric.put("PL", "616"); name.put("PL", "Poland"); phone.put("PL", "+48");
        iso3.put("PT", "PRT"); numeric.put("PT", "620"); name.put("PT", "Portugal"); phone.put("PT", "+351");
        iso3.put("PR", "PRI"); numeric.put("PR", "630"); name.put("PR", "Puerto Rico"); phone.put("PR", "+1-787, +1-939");
        iso3.put("QA", "QAT"); numeric.put("QA", "634"); name.put("QA", "Qatar"); phone.put("QA", "+974");
        iso3.put("RE", "REU"); numeric.put("RE", "638"); name.put("RE", "Réunion"); phone.put("RE", "+262");
        iso3.put("RO", "ROU"); numeric.put("RO", "642"); name.put("RO", "Romania"); phone.put("RO", "+40");
        iso3.put("RU", "RUS"); numeric.put("RU", "643"); name.put("RU", "Russian Federation"); phone.put("RU", "+7");
        iso3.put("RW", "RWA"); numeric.put("RW", "646"); name.put("RW", "Rwanda"); phone.put("RW", "+250");
        iso3.put("BL", "BLM"); numeric.put("BL", "652"); name.put("BL", "Saint Barthélemy"); phone.put("BL", "+590");
        iso3.put("SH", "SHN"); numeric.put("SH", "654"); name.put("SH", "Saint Helena, Ascension and Tristan da Cunha"); phone.put("SH", "+290");
        iso3.put("KN", "KNA"); numeric.put("KN", "659"); name.put("KN", "Saint Kitts and Nevis"); phone.put("KN", "+1-869");
        iso3.put("LC", "LCA"); numeric.put("LC", "662"); name.put("LC", "Saint Lucia"); phone.put("LC", "+1-758");
        iso3.put("MF", "MAF"); numeric.put("MF", "663"); name.put("MF", "Saint Martin (French part)"); phone.put("MF", "+590");
        iso3.put("PM", "SPM"); numeric.put("PM", "666"); name.put("PM", "Saint Pierre and Miquelon"); phone.put("PM", "+508");
        iso3.put("VC", "VCT"); numeric.put("VC", "670"); name.put("VC", "Saint Vincent and the Grenadines"); phone.put("VC", "+1-784");
        iso3.put("WS", "WSM"); numeric.put("WS", "882"); name.put("WS", "Samoa"); phone.put("WS", "+685");
        iso3.put("SM", "SMR"); numeric.put("SM", "674"); name.put("SM", "San Marino"); phone.put("SM", "+378");
        iso3.put("ST", "STP"); numeric.put("ST", "678"); name.put("ST", "Sao Tome and Principe"); phone.put("ST", "+239");
        iso3.put("SA", "SAU"); numeric.put("SA", "682"); name.put("SA", "Saudi Arabia"); phone.put("SA", "+966");
        iso3.put("SN", "SEN"); numeric.put("SN", "686"); name.put("SN", "Senegal"); phone.put("SN", "+221");
        iso3.put("RS", "SRB"); numeric.put("RS", "688"); name.put("RS", "Serbia"); phone.put("RS", "+381");
        iso3.put("SC", "SYC"); numeric.put("SC", "690"); name.put("SC", "Seychelles"); phone.put("SC", "+248");
        iso3.put("SL", "SLE"); numeric.put("SL", "694"); name.put("SL", "Sierra Leone"); phone.put("SL", "+232");
        iso3.put("SG", "SGP"); numeric.put("SG", "702"); name.put("SG", "Singapore"); phone.put("SG", "+65");
        iso3.put("SX", "SXM"); numeric.put("SX", "534"); name.put("SX", "Sint Maarten (Dutch part)"); phone.put("SX", "+1-721");
        iso3.put("SK", "SVK"); numeric.put("SK", "703"); name.put("SK", "Slovakia"); phone.put("SK", "+421");
        iso3.put("SI", "SVN"); numeric.put("SI", "705"); name.put("SI", "Slovenia"); phone.put("SI", "+386");
        iso3.put("SB", "SLB"); numeric.put("SB", "090"); name.put("SB", "Solomon Islands"); phone.put("SB", "+677");
        iso3.put("SO", "SOM"); numeric.put("SO", "706"); name.put("SO", "Somalia"); phone.put("SO", "+252");
        iso3.put("ZA", "ZAF"); numeric.put("ZA", "710"); name.put("ZA", "South Africa"); phone.put("ZA", "+27");
        iso3.put("GS", "SGS"); numeric.put("GS", "239"); name.put("GS", "South Georgia and the South Sandwich Islands"); phone.put("GS", "+500");
        iso3.put("SS", "SSD"); numeric.put("SS", "728"); name.put("SS", "South Sudan"); phone.put("SS", "+211");
        iso3.put("ES", "ESP"); numeric.put("ES", "724"); name.put("ES", "Spain"); phone.put("ES", "+34");
        iso3.put("LK", "LKA"); numeric.put("LK", "144"); name.put("LK", "Sri Lanka"); phone.put("LK", "+94");
        iso3.put("SD", "SDN"); numeric.put("SD", "729"); name.put("SD", "Sudan"); phone.put("SD", "+249");
        iso3.put("SR", "SUR"); numeric.put("SR", "740"); name.put("SR", "Suriname"); phone.put("SR", "+597");
        iso3.put("SJ", "SJM"); numeric.put("SJ", "744"); name.put("SJ", "Svalbard and Jan Mayen"); phone.put("SJ", "+47");
        iso3.put("SE", "SWE"); numeric.put("SE", "752"); name.put("SE", "Sweden"); phone.put("SE", "+46");
        iso3.put("CH", "CHE"); numeric.put("CH", "756"); name.put("CH", "Switzerland"); phone.put("CH", "+41");
        iso3.put("SY", "SYR"); numeric.put("SY", "760"); name.put("SY", "Syrian Arab Republic"); phone.put("SY", "+963");
        iso3.put("TW", "TWN"); numeric.put("TW", "158"); name.put("TW", "Taiwan, Province of China"); phone.put("TW", "+886");
        iso3.put("TJ", "TJK"); numeric.put("TJ", "762"); name.put("TJ", "Tajikistan"); phone.put("TJ", "+992");
        iso3.put("TZ", "TZA"); numeric.put("TZ", "834"); name.put("TZ", "Tanzania, United Republic of"); phone.put("TZ", "+255");
        iso3.put("TH", "THA"); numeric.put("TH", "764"); name.put("TH", "Thailand"); phone.put("TH", "+66");
        iso3.put("TL", "TLS"); numeric.put("TL", "626"); name.put("TL", "Timor-Leste"); phone.put("TL", "+670");
        iso3.put("TG", "TGO"); numeric.put("TG", "768"); name.put("TG", "Togo"); phone.put("TG", "+228");
        iso3.put("TK", "TKL"); numeric.put("TK", "772"); name.put("TK", "Tokelau"); phone.put("TK", "+690");
        iso3.put("TO", "TON"); numeric.put("TO", "776"); name.put("TO", "Tonga"); phone.put("TO", "+676");
        iso3.put("TT", "TTO"); numeric.put("TT", "780"); name.put("TT", "Trinidad and Tobago"); phone.put("TT", "+1-868");
        iso3.put("TN", "TUN"); numeric.put("TN", "788"); name.put("TN", "Tunisia"); phone.put("TN", "+216");
        iso3.put("TR", "TUR"); numeric.put("TR", "792"); name.put("TR", "Turkey"); phone.put("TR", "+90");
        iso3.put("TM", "TKM"); numeric.put("TM", "795"); name.put("TM", "Turkmenistan"); phone.put("TM", "+993");
        iso3.put("TC", "TCA"); numeric.put("TC", "796"); name.put("TC", "Turks and Caicos Islands"); phone.put("TC", "+1-649");
        iso3.put("TV", "TUV"); numeric.put("TV", "798"); name.put("TV", "Tuvalu"); phone.put("TV", "+688");
        iso3.put("UG", "UGA"); numeric.put("UG", "800"); name.put("UG", "Uganda"); phone.put("UG", "+256");
        iso3.put("UA", "UKR"); numeric.put("UA", "804"); name.put("UA", "Ukraine"); phone.put("UA", "+380");
        iso3.put("AE", "ARE"); numeric.put("AE", "784"); name.put("AE", "United Arab Emirates"); phone.put("AE", "+971");
        iso3.put("GB", "GBR"); numeric.put("GB", "826"); name.put("GB", "United Kingdom of Great Britain and Northern Ireland"); phone.put("GB", "+44");
        iso3.put("UM", "UMI"); numeric.put("UM", "581"); name.put("UM", "United States Minor Outlying Islands"); phone.put("UM", "+1");
        iso3.put("US", "USA"); numeric.put("US", "840"); name.put("US", "United States of America"); phone.put("US", "+1");
        iso3.put("UY", "URY"); numeric.put("UY", "858"); name.put("UY", "Uruguay"); phone.put("UY", "+598");
        iso3.put("UZ", "UZB"); numeric.put("UZ", "860"); name.put("UZ", "Uzbekistan"); phone.put("UZ", "+998");
        iso3.put("VU", "VUT"); numeric.put("VU", "548"); name.put("VU", "Vanuatu"); phone.put("VU", "+678");
        iso3.put("VE", "VEN"); numeric.put("VE", "862"); name.put("VE", "Venezuela (Bolivarian Republic of)"); phone.put("VE", "+58");
        iso3.put("VN", "VNM"); numeric.put("VN", "704"); name.put("VN", "Viet Nam"); phone.put("VN", "+84");
        iso3.put("VG", "VGB"); numeric.put("VG", "092"); name.put("VG", "Virgin Islands (British)"); phone.put("VG", "+1-284");
        iso3.put("VI", "VIR"); numeric.put("VI", "850"); name.put("VI", "Virgin Islands (U.S.)"); phone.put("VI", "+1-340");
        iso3.put("WF", "WLF"); numeric.put("WF", "876"); name.put("WF", "Wallis and Futuna"); phone.put("WF", "+681");
        iso3.put("EH", "ESH"); numeric.put("EH", "732"); name.put("EH", "Western Sahara"); phone.put("EH", "+212");
        iso3.put("YE", "YEM"); numeric.put("YE", "887"); name.put("YE", "Yemen"); phone.put("YE", "+967");
        iso3.put("ZM", "ZMB"); numeric.put("ZM", "894"); name.put("ZM", "Zambia"); phone.put("ZM", "+260");
        iso3.put("ZW", "ZWE"); numeric.put("ZW", "716"); name.put("ZW", "Zimbabwe"); phone.put("ZW", "+263");



        // Some territories or multi-code entries may have multiple phone codes; we stored as comma-separated strings above.

        ISO2_TO_ISO3 = Collections.unmodifiableMap(iso3);

        // numeric map: values set above inline for entries
        ISO2_TO_NUMERIC = Collections.unmodifiableMap(numeric);

        // name map: if any names weren't set above, fallback using iso3 (rare)
        Map<String, String> tempName = new HashMap<>();
        // many name.put were set inline above; ensure all iso3 keys have a name entry:
        for (String k : ISO2_TO_ISO3.keySet()) {
            tempName.put(k, name.getOrDefault(k, ISO2_TO_ISO3.get(k)));
        }
        ISO2_TO_NAME = Collections.unmodifiableMap(tempName);

        // phone map: many phone.put set inline above
        Map<String, String> tempPhone = new HashMap<>();
        for (String k : ISO2_TO_ISO3.keySet()) {
            tempPhone.put(k, phone.getOrDefault(k, ""));
        }
        ISO2_TO_PHONE = Collections.unmodifiableMap(tempPhone);
    }
    /**
     * Retrieve ISO-3 (alpha-3) code for a given ISO-2 code.
     *
     * @param iso2 ISO-3166-1 alpha-2 code (case-insensitive)
     * @return ISO-3166-1 alpha-3 code, or empty string if not found
     */
    public static String getIso3(final String iso2) {
        if (iso2 == null) {
            return "";
        }
        return ISO2_TO_ISO3.getOrDefault(iso2.toUpperCase(), "");
    }

    /**
     * Retrieve numeric ISO code for a given ISO-2 code.
     *
     * @param iso2 ISO-3166-1 alpha-2 code (case-insensitive)
     * @return numeric code as string (zero-padded per ISO), or empty string if not found
     */
    public static String getNumericCode(final String iso2) {
        if (iso2 == null) {
            return "";
        }
        return ISO2_TO_NUMERIC.getOrDefault(iso2.toUpperCase(), "");
    }

    /**
     * Retrieve a common short country name for a given ISO-2 code.
     *
     * @param iso2 ISO-3166-1 alpha-2 code (case-insensitive)
     * @return country name, or empty string if not found
     */
    public static String getCountryName(final String iso2) {
        if (iso2 == null) {
            return "";
        }
        return ISO2_TO_NAME.getOrDefault(iso2.toUpperCase(), "");
    }

    /**
     * Retrieve the international dialing (phone) code for a given ISO-2 code.
     *
     * Values include '+' and may contain comma-separated alternatives for countries with multiple codes.
     *
     * @param iso2 ISO-3166-1 alpha-2 code (case-insensitive)
     * @return phone code string (e.g. "+1", "+44"), or empty string if not known
     */
    public static String getPhoneCode(final String iso2) {
        if (iso2 == null) {
            return "";
        }
        return ISO2_TO_PHONE.getOrDefault(iso2.toUpperCase(), "");
    }

    /**
     * Returns whether the provided ISO-2 code is known in the dataset.
     *
     * @param iso2 ISO-3166-1 alpha-2 code (case-insensitive)
     * @return true if known, false otherwise
     */
    public static boolean isKnownIso2(final String iso2) {
        if (iso2 == null) {
            return false;
        }
        return ISO2_TO_ISO3.containsKey(iso2.toUpperCase());
    }

    /**
     * Number of known ISO-2 entries in the embedded dataset.
     *
     * @return size of internal mapping
     */
    public static int knownCount() {
        return ISO2_TO_ISO3.size();
    }

}
